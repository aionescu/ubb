create or alter procedure randomInt @min int, @max int, @number int output as
	set @number = (select floor(rand() * (@max - @min + 1) + @min))
go

create or alter procedure randomDate @date date output as
	set @date = dateAdd(day, abs(checksum(newID()) % 3650), '2000-01-01')
go

create or alter procedure randomVarchar @length int, @string varchar(50) output as
	set @string = ''
	while @length > 0 begin
		set @string = @string + char(floor(rand() * (122 - 97 + 1) + 97))
		set @length = @length - 1
	end
go

create or alter procedure getColumnNames
	@tableName varchar(150),
	@tableColumns varchar(2000) output
as
	set @tableColumns = ''
	declare @columnName varchar(150), @dataType varchar(150)
	declare @schemaTableName varchar(150) = concat('dbo.', @tableName)

	declare c cursor local
	for
    select column_name
    from information_schema.columns
    where table_name = @tableName
    order by column_name asc

	open c
	fetch next from c into @columnName

	while @@FETCH_STATUS = 0 begin
    if columnProperty(object_id(@schemaTableName), @columnName, 'IsIdentity') = 0
      set @tableColumns = (concat(@tableColumns, '[' + @columnName + '], '))

    fetch next from c into @columnName
	end

	close c
	deallocate c

	set @tableColumns = substring(@tableColumns, 1, len(@tableColumns) - 1)
go

create or alter procedure isColumnForeignKey
	@columnName varchar(150),
	@tableName varchar(150)
as
	if exists (
    select *
		from (
      select [name] as foreignKey
      from sys.foreign_keys as FK
      where parent_object_id = object_id(@tableName)
    ) as FKs
		where charIndex(@columnName, FKs.foreignKey) > 0
	) return 1

	return 0
go

create or alter procedure pickRandomForeignKey
	@tableName varchar(150),
	@columnName varchar(150)
as
	declare @refTable varchar(150)
	declare @refColumn varchar(150)
	declare fkCursor cursor for
		select object_name(referenced_object_id), ColumnName = (
			select c.Column_Name from
        INFORMATION_SCHEMA.TABLE_CONSTRAINTS T,
        INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE c
			where
				c.Constraint_Name = T.Constraint_Name
				and c.Table_Name = T.Table_Name
				and Constraint_Type = 'primary key'
				and c.Table_Name = OBJECT_NAME(referenced_object_id)
			)
		from sys.foreign_keys as FK
		where parent_object_id = object_id(@tableName) and charIndex(@columnName, FK.[name]) > 0

	open fkCursor
	fetch fkCursor into @refTable, @refColumn

	close fkCursor
	deallocate fkCursor

	declare @value int
	declare @query nvarchar(100) = N'set @value = (select top 1 (' + @refColumn + ') from ' + @refTable + ' order by NEWID())'
	execute sp_executesql @query, N'@value int output', @value = @value output

	return cast(@value as varchar(100))
go

create or alter procedure insertRandomRow
	@tableName varchar(150),
	@tableColumns varchar(2000)
as
	declare @tableValues varchar(2000) = ''
	declare @columnName varchar(150), @dataType varchar(150)
	declare @schemaTableName varchar(150) = concat('dbo.', @tableName)
	declare @isForeignKey int
	declare @value varchar(150)

	declare c cursor local
	for
    select column_name, data_type
    from information_schema.columns
    where table_name = @tableName
    order by column_name asc

	open c
	fetch next from c into @columnName, @dataType

	while @@FETCH_STATUS = 0 begin
    exec @isForeignKey = isColumnForeignKey @columnName=@columnName, @tableName=@tableName

    if columnProperty(object_id(@schemaTableName), @columnName, 'IsIdentity') = 0 and @isForeignKey = 0 begin
      if @dataType = 'int' begin
        declare @int int
        exec randomInt 0, 10000, @int output
        set @tableValues = concat(@tableValues, cast(@int as varchar(100)) + ', ')
      end
      else if (@dataType = 'nvarchar' OR @dataType = 'nchar' OR @dataType = 'varchar' OR @dataType = 'char') begin
        declare @varchar varchar(100)
        exec randomVarchar 20, @varchar output
        set @tableValues = concat(@tableValues, '''' + cast(@varchar as varchar) + ''', ')
      end
      else if (@dataType = 'date') begin
        declare @date date
        exec randomDate @date output
        set @tableValues = concat(@tableValues, '''' + cast(@date as varchar) + ''', ')
      end
      else
        set @tableValues = concat(@tableValues, 'cast('''' as ' + @dataType + '), ')
    end

    if @isForeignKey = 1 begin
      exec @value = pickRandomForeignKey @tableName=@tableName, @columnName=@columnName
      set @tableValues = concat(@tableValues, @value + ', ')
    end

    fetch next from c into @columnName, @dataType
	end

	close c
	deallocate c

	set @tableValues = substring(@tableValues, 1, len(@tableValues) - 1)
	exec ('insert into ' + @tableName + ' (' + @tableColumns + ') values (' + @tableValues + ')')
go

create or alter procedure prepareTest
	@tableInfo varchar(2000),
	@viewInfo varchar(2000),
	@testName varchar(100)
as
	declare @tableName varchar(100)
	declare @numRows int
	declare @pos int
	declare @shouldStop int = 0
	declare @tableID int = 0
	declare @position int = 1

	insert into Tests([Name]) values (@testName)

  -- Last identity value in the batch -> Last record added in the Tests table
	declare @testID int = scope_identity()

	while @shouldStop = 0 begin
		set @pos = charIndex(' ', @tableInfo)
		set @tableName = substring(@tableInfo, 1, @pos - 1)
		set @tableInfo = substring(@tableInfo, @pos + 1, 9999)
		set @pos = charIndex(' ', @tableInfo)

		if @pos = 0 begin
			set @pos = 99999
			set @shouldStop = 1
		end

		set @numRows = cast(substring(@tableInfo, 1, @pos) as int)

		if @shouldStop = 0 begin
			set @pos = charIndex(' ', @tableInfo)
			set @tableInfo = substring(@tableInfo, @pos + 1, 9999)
		end

		if not exists (select * from [Tables] where [Name]=@tableName)
			insert into [Tables]([Name]) values (@tableName)

		set @tableID = (select TableID from [Tables] where [Name] = @tableName)
		insert into TestTables(TestID, TableID, NoOfRows, Position) values (@testID, @tableID, @numRows, @position)

		set @position = @position + 1
	end

	-- Adding the views
	declare @viewName varchar(100)
	declare @viewID int

	set @shouldStop = 0

	while @shouldStop = 0 begin
		set @pos = charIndex(' ', @viewInfo)

		if @pos = 0 begin
			set @pos = 99999
			set @shouldStop = 1
		end

		set @viewName = substring(@viewInfo, 1, @pos - 1)
		-- print('View name: ' + @viewName)

		if @shouldStop = 0 begin
			set @pos = charIndex(' ', @viewInfo)
			set @viewInfo = substring(@viewInfo, @pos + 1, 9999)
		end

		if not exists (select * from [Views] where [Name]=@viewName)
			insert into [Views]([Name]) values (@viewName)

		set @viewID = (select ViewID from [Views] where [Name]=@viewName)
		insert into TestViews(TestID, ViewID) values (@testID, @viewID)
	end

	return @testID
go

create or alter procedure insertRandomRows
	@tableName varchar(100),
	@count int
as
	declare @tableColumns varchar(1000)
	declare @tableValues varchar(1000) = ''
	declare @nextVal int = 1

	exec getColumnNames @tableName = @tableName, @tableColumns = @tableColumns output

	while @nextVal <= @count begin
		exec insertRandomRow @tableName = @tableName, @tableColumns = @tableColumns
		set @nextVal = @nextVal + 1
	end
go

create or alter procedure runTest
	@tableInfo varchar(2000),
	@viewInfo varchar(2000),
	@testName varchar(100)
as
	declare @testID int
	exec @testID = prepareTest @tableInfo = @tableInfo, @viewInfo = @viewInfo, @testName = @testName

	declare @tableID int, @numRows int, @name varchar(150)
	declare @start datetime2, @end datetime2

	insert into TestRuns(Description, StartAt, EndAt) values (str(@testID), sysDateTime(), '')

	declare @testRunID int = scope_identity()

	declare d cursor local for
    select T.[Name] as TableName
    from TestTables as TT
    inner join [Tables] as T
    on T.TableId = TT.TableId
    where TestID = @testID
    order by Position desc

    open d
    fetch next from d into @name

    while @@FETCH_STATUS = 0 begin
      exec ('delete from ' + @name)
      fetch next from d into @name
    end

	close d
	deallocate d

	declare c cursor local for
    select T.TableId, NoOfRows, T.[Name] as TableName
    from TestTables as TT
    inner join [Tables] as T
    on T.TableId = TT.TableId
    where TestID = @testID
    order by Position asc

	open c
	fetch next from c into @tableID, @numRows, @name

	while @@FETCH_STATUS = 0 begin
    set @start = sysDateTime()
    exec insertRandomRows @tableName = @name, @count = @numRows

    set @end = sysDateTime()
    insert into TestRunTables(TestRunID, TableID, StartAt, EndAt) values (@testRunID, @tableID, @start, @end)

    fetch next from c into @tableID, @numRows, @name
	end

	close c
	deallocate c

	declare @viewID int

	declare c cursor local for
    select V.Name, V.ViewID
    from TestViews as TV
    inner join [Views] as V on V.ViewID = TV.ViewID
    where TestId = @testID

	open c
	fetch next from c into @name, @viewID

	while @@FETCH_STATUS = 0 begin
    set @start = sysDateTime()
    -- exec ('select * from ' + @name)

    set @end = sysDateTime()
    insert into TestRunViews(TestRunID, ViewID, StartAt, EndAt) values (@testRunID, @viewID, @start, @end)

    fetch next from c into @name, @viewID
	end

	close c
	deallocate c

	update TestRuns set EndAt = sysDateTime() where TestRunID = @testRunID
go

if exists
( select name
  from sys.tables
  where name = 'Maintainers'
)
begin
  drop table PkgVersions
  drop table Pkgs
  drop table Maintainers
end

go

create table Maintainers
  ( maintainerID int primary key identity
  ,	maintainerName varchar(30)
  ,	signupDate date
  )

create table Pkgs
  ( pkgID int primary key identity
	, pkgName varchar(30)
  , maintainer int foreign key references Maintainers(maintainerID)
	, lastVersionUploadDate date
  )

create table PkgVersions
  ( versionID int identity
	, downloadCount int
	, uploadDate date
	, primary key (versionID, downloadCount)
  )

go

create or alter view AllMaintainersView as
  select * from Maintainers
go

create or alter view PkgMaintainersView as
  select maintainerName, pkgName
  from Maintainers
  inner join Pkgs
  on maintainerID = maintainer
go

create or alter view PkgVersionsView as
  select dateCol, count(*) as 'Count'
  from (
    select versionID as idCol, uploadDate as dateCol
    from PkgVersions as PV
    union
    select pkgID as idCol, lastVersionUploadDate as dateCol
    from Pkgs as P
  ) as PVV
  group by dateCol
go

exec runTest
  @tableInfo = 'Maintainers 100 Pkgs 100 PkgVersions 100',
  @viewInfo = 'AllMaintainersView PkgMaintainersView PkgVersionsView',
  @testName = 'The Test'

select * from Maintainers
select * from Pkgs
select * from PkgVersions

select * from AllMaintainersView
select * from PkgMaintainersView
select * from PkgVersionsView

select * from Tables
select * from Views

select * from Tests
select * from TestRuns

select * from TestTables
select * from TestViews

select * from TestRunTables
select * from TestRunViews
