create or alter procedure randomInt(@min int, @max int, @number int output) as begin
	set @number = (select FLOOR(RAND()*(@max-@min+1)+@min))
end
go

create or alter procedure randomDate (@date DATE output) as begin
	set @date = DATEADD(DAY, ABS(CHECKSUM(NEWID()) % 3650), '2000-01-01')
end
go

create or alter procedure randomVarchar(@length int, @string VARCHAR(50) output) as begin
	set @string = ''
	while @length > 0 begin
		set @string = @string + char(FLOOR(RAND()*(122-97+1)+97))
		set @length = @length - 1;
	end
end
go

create or alter procedure getColumnNames
	@tableName varchar(150),
	@tableColumns varchar(2000) output
as
	set @tableColumns = ''
	declare @columnName varchar(150), @DataType varchar(150)
	declare @schemaTableName varchar(150) = CONCAT('dbo.', @tableName)

	declare C cursor local
	for select COLUMN_NAME from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME=@tableName order by COLUMN_NAME asc;

	open C
	fetch next from C into @columnName
	while @@FETCH_STATUS = 0
		begin
			if COLUMNPROPERTY(OBJECT_ID(@schemaTableName), @columnName, 'IsIdentity') = 0
			begin
				set @tableColumns = (CONCAT(@tableColumns, '[' + @columnName + '], '))
			end
			fetch next from C into @columnName
		end
	close C
	deallocate C

	set @tableColumns = SUBSTRING(@tableColumns, 1, LEN(@tableColumns)-1)
go

create or alter procedure isColumnForeignKey
	@columnName varchar(150),
	@tableName varchar(150)
as
	if exists
	(
		select *
		from (select [name] as Foreign_Key
				from sys.foreign_keys as FK
				where parent_object_id = OBJECT_ID(@tableName)) as FKs
		where CHARINDEX(@columnName, FKs.Foreign_Key) > 0
	) return 1
  
	return 0
go

create or alter procedure pickRandomForeignKey
	@tableName varchar(150),
	@columnName varchar(150)
as
	declare @RefTable varchar(150)
	declare @RefColumn varchar(150)
	declare ForeignKeyCursor cursor FOR 
		select OBJECT_NAME(referenced_object_id), ColumnName = (
			select C.Column_Name from
			INFORMATION_SCHEMA.TABLE_CONSTRAINTS T,
			INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE C
			where
				C.Constraint_Name = T.Constraint_Name 
				and C.Table_Name = T.Table_Name
				and Constraint_Type = 'primary key'
				and C.Table_Name = OBJECT_NAME(referenced_object_id)
			)
		from sys.foreign_keys as FK
		where parent_object_id = OBJECT_ID(@tableName) and CHARINDEX(@columnName, FK.[name]) > 0

	open ForeignKeyCursor

	fetch ForeignKeyCursor
	into @RefTable, @RefColumn

	close ForeignKeyCursor
	deallocate ForeignKeyCursor

	declare @Value int
	declare @SqlCommand nvarchar(100) = N'set @Value = (select top 1 (' + @RefColumn + ') from ' + @RefTable + ' order by NEWID())'
	execute sp_executesql @SqlCommand, N'@Value int output', @Value = @Value output;

	return CAST(@Value as VARCHAR(100))
go

create or alter procedure insertRandomRow
	@tableName varchar(150),
	@tableColumns varchar(2000)
as
	declare @TableValues varchar(2000) = ''
	declare @columnName varchar(150), @DataType varchar(150)
	declare @schemaTableName varchar(150) = CONCAT('dbo.', @tableName)
	declare @IsForeignKey int
	declare @Value varchar(150)
	--
	declare C cursor local
	for select COLUMN_NAME, DATA_TYPE from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME=@tableName order by COLUMN_NAME asc;
	open C
	fetch next from C into @columnName, @DataType
	while @@FETCH_STATUS = 0
		begin
			exec @IsForeignKey = isColumnForeignKey @columnName=@columnName, @tableName=@tableName
			if COLUMNPROPERTY(OBJECT_ID(@schemaTableName), @columnName, 'IsIdentity') = 0 and @IsForeignKey = 0
			begin
				if @DataType = 'int'
				begin
					declare @int int
					exec randomInt 0, 10000, @int output 
					set @TableValues = CONCAT(@TableValues, CAST(@int as VARCHAR(100)) + ', ')
				end
				ELSE
					if (@DataType = 'nvarchar' OR @DataType = 'nchar' OR @DataType = 'varchar' OR @DataType = 'char')
						begin
							declare @varchar varchar(100)
							exec randomVarchar 20, @varchar output 
							set @TableValues = CONCAT(@TableValues, '''' + CAST(@varchar as VARCHAR) + ''', ')
						end
					ELSE 
						if (@DataType = 'date')
						begin
							declare @date date
							exec randomDate @date output 
							set @TableValues = CONCAT(@TableValues, '''' + CAST(@date as VARCHAR) + ''', ')
						end
						ELSE
							set @TableValues = CONCAT(@TableValues, 'CAST('''' as ' + @DataType + '), ') 
			end
			
			if @IsForeignKey = 1
			begin
				exec @Value = pickRandomForeignKey @tableName=@tableName, @columnName=@columnName
				set @TableValues = CONCAT(@TableValues, @Value + ', ')
			end
			fetch next from C into @columnName, @DataType
		end
	close C
	deallocate C
	--
	set @TableValues = SUBSTRING(@TableValues, 1, LEN(@TableValues) - 1)
	exec ('insert into ' + @tableName + ' (' + @tableColumns + ') values (' + @TableValues + ')');
go

create or alter procedure prepareTest
	@tableInfo varchar(2000),
	@viewInfo varchar(2000),
	@testName varchar(100)
as
	-- Adding the tables
	declare @tableName varchar(100)
	declare @NoRows int
	declare @Pos int
	declare @ShouldStop int = 0
	declare @TableId int = 0
	declare @Position int = 1
	insert into Tests ([Name]) values (@testName)
	declare @TestID int = SCOPE_IDENTITY(); -- get the last identity value in the batch -> the las record added in the Tests table
	while @ShouldStop = 0 --
	begin
		set @Pos = CHARINDEX(' ', @tableInfo)
		set @tableName = SUBSTRING(@tableInfo, 1, @Pos - 1)
		set @tableInfo = SUBSTRING(@tableInfo, @Pos + 1, 9999);
		set @Pos = CHARINDEX(' ', @tableInfo)
		if @Pos = 0 -- if we processed the last field in the string, stop.
		begin
			set @Pos = 99999
			set @ShouldStop = 1
		end
		set @NoRows = CAST(SUBSTRING(@tableInfo, 1, @Pos) as int)
		if @ShouldStop = 0
		begin
			set @Pos = CHARINDEX(' ', @tableInfo)
			set @tableInfo = SUBSTRING(@tableInfo, @Pos + 1, 9999);
		end
		-- Got the table name and no of rows;
		-- inserting table if doesn't exist already
		if NOT exists(select * from [Tables] where [Name]=@tableName)
			insert into [Tables] ([Name]) values (@tableName)
		set @TableId = (select TableID from [Tables] where [Name]=@tableName)
		insert into TestTables (TestID, TableID, NoOfRows, Position) values (@TestID, @TableId, @NoRows, @Position);
		set @Position = @Position + 1
	end
	

	-- Adding the views
	declare @ViewName varchar(100)
	declare @ViewId int
	set @ShouldStop = 0
	while @ShouldStop = 0
	begin
		set @Pos = CHARINDEX(' ', @viewInfo)
		if @Pos = 0
		begin
			set @Pos = 99999
			set @ShouldStop = 1
		end
		set @ViewName = SUBSTRING(@viewInfo, 1, @Pos - 1)
		PRINT(@ViewName)
		if @ShouldStop = 0
		begin
			set @Pos = CHARINDEX(' ', @viewInfo)
			set @viewInfo = SUBSTRING(@viewInfo, @Pos + 1, 9999);
		end
		if NOT exists(select * from [Views] where [Name]=@ViewName) 
			insert into [Views] ([Name]) values (@ViewName)
		set @ViewId = (select ViewID from [Views] where [Name]=@ViewName)
		insert into TestViews (TestID, ViewID) values (@TestID, @ViewId);
	end

	return @TestId
go

create or alter procedure insertRandomRows
	@tableName varchar(100),
	@count int
as
	declare @tableColumns varchar(1000)
	declare @TableValues varchar(1000) = ''
	declare @NextVal int = 1
	--
	exec dbo.getColumnNames @tableName = @tableName, @tableColumns = @tableColumns output;
	--
	
	while @NextVal <= @count
	begin
		exec insertRandomRow @tableName = @tableName, @tableColumns = @tableColumns
		set @NextVal = @NextVal + 1
	end
go

create or alter procedure runTest
	@tableInfo varchar(2000),
	@viewInfo varchar(2000),
	@testName varchar(100)
as
	declare @TestId int
	exec @TestId = prepareTest @tableInfo = @tableInfo, @viewInfo = @viewInfo, @testName = @testName
	declare @TableId int, @NoOfRows int, @Name varchar(150)
	declare @Start datetime2, @End datetime2;
	--
	insert into TestRuns (Description, StartAt, EndAt) values (STR(@TestID), SYSDATETIME(), '');
	declare @TestRunID int = SCOPE_IDENTITY();
	--
	declare D cursor local
	for select T.[Name] as TableName
	from TestTables as TT
	inner join [Tables] as T on T.TableId = TT.TableId
	where TestID = @TestID 
	order by Position DESC;
	open D
	fetch next from D into @Name
	while @@FETCH_STATUS = 0
		begin
			exec ('DELETE from ' + @Name);
			fetch next from D into @Name
		end
	close D
	deallocate D

	declare C cursor local
	for select T.TableId, NoOfRows, T.[Name] as TableName
	from TestTables as TT
	inner join [Tables] as T on T.TableId = TT.TableId
	where TestID = @TestID 
	order by Position asc;
	open C
	fetch next from C into @TableId, @NoOfRows, @Name
	while @@FETCH_STATUS = 0
		begin
			set @Start = SYSDATETIME()
			exec insertRandomRows @tableName = @Name, @count = @NoOfRows
			set @End = SYSDATETIME()
			insert into TestRunTables (TestRunID, TableID, StartAt, EndAt) values (@TestRunID, @TableId, @Start, @End);
			fetch next from C into @TableId, @NoOfRows, @Name
		end
	close C
	deallocate C

	declare @ViewId int
	declare C cursor local
	for select V.Name, V.ViewID
	from TestViews as TV
	inner join [Views] as V on V.ViewID = TV.ViewID
	where TestId = @TestID;
	open C
	fetch next from C into @Name, @ViewId
	while @@FETCH_STATUS = 0
		begin
			set @Start = SYSDATETIME()
			exec ('select * from ' + @Name);
			set @End = SYSDATETIME()
			insert into TestRunViews (TestRunID, ViewID, StartAt, EndAt) values (@TestRunID, @ViewId, @Start, @End);
			fetch next from C into @Name, @ViewId
		end
	close C
	deallocate C

	update TestRuns set EndAt = SYSDATETIME() where TestRunID = @TestRunID;
go

if exists
( select name
  from sys.tables
  where name = 'FirstTable'
)
begin
  drop table ThirdTable
  drop table SecondTable
  drop table FirstTable
end

go

create table FirstTable
  ( f1 int primary key identity
  ,	f2 varchar(30)
  ,	f3 date
  )

create table SecondTable
  ( s1 int primary key identity
	, s2 int
	, s3 date
	, constraint FK_s2 foreign key (s2)
    references FirstTable(f1) 
  )

create table ThirdTable
  ( t1 int identity
	, t2 nchar(30)
	, t3 date
	, primary key (t1, t2)
  )

go

create or alter view FirstView as
select * from FirstTable;
go

create or alter view SecondView as
select FT.f1, SC.s3
from FirstTable as FT
inner join SecondTable as SC on FT.f1 = SC.s2
go

create or alter view ThirdView as
select F.[Date], COUNT(*) as 'Count'
from (
	select t1 as [Number], t3 as [Date]
	from ThirdTable as TT
	union 
	select s2 as [Number], s3 as [Date]
	from SecondTable as ST
) as F
group by F.Date
go

exec runTest @tableInfo='FirstTable 10 SecondTable 10 ThirdTable 10', @viewInfo='FirstView SecondView ThirdView', @testName='The Test';

select * from SecondTable

select * from TestTables

select * from TestRuns
select * from TestRunTables
select * from TestRunViews
