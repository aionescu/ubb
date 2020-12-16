if exists
( select name
  from sys.tables
  where name = 'Ta'
)
begin
  drop table Tc
  drop table Tb
  drop table Ta
end

create table Ta
  (	aid int primary key identity
  ,	a2 int unique
  ,	a3 int
  )

create table Tb
  ( bid int primary key identity
  ,	b2 int
  ,	b3 int
  )

create table Tc
  (	cid int primary key identity
  ,	aid int references Ta(aid)
  ,	bid int references Tb(bid)
  )

go

create or alter procedure populateTable @table varchar(30) as
  declare @query varchar(200)

  declare @counter int
  set @counter = 1

  while @counter <= 200  begin
    set @query =
      'insert into ' + @table
      + ' values (' + convert(varchar, @counter)
      + ', ' + convert(varchar, @counter) + ')'

    exec (@query)
    set @counter = @counter + 1
  end
go

exec populateTable Ta
exec populateTable Tb
exec populateTable Tc

go

-- a)

-- Clustered index scan
select aid, a3
from Ta
where a3 = 90

-- Clustered index seek
select *
from Ta
where aid between 1 and 110

-- Nonclustered index scan
select a2
from Ta

-- Nonclustered index seek
select a2
from Ta
where a2 = 80

-- Key lookup
-- Nonclustered index seek 
select *
from Ta
where a2 = 120

-- b)

if exists
( select *
  from sys.indexes
  where name = 'Ix_Tb_b2'
) drop index Ix_Tb_b2 on Tb

-- Clustered scan
select bid
from Tb
where b2 = 97

if exists
( select * from
  sys.indexes
  where name = 'Ix_Tb_b2'
) drop index Ix_Tb_b2 on Tb

create nonclustered index Ix_Tb_b2 on Tb(b2)

-- Nonclustered scan
select bid
from Tb
where b2 = 98

go

-- c)

create or alter view TestView as 
	select C.cid
	from Tc C
  left join Ta A
  on C.aid = A.aid
	where C.aid between 18 and 420
go

-- Clustered seek
select *
from TestView 

if exists
( select * from
  sys.indexes
  where name = 'Ix_Tc_aid'
) drop index Ix_Tc_aid on Tc

create nonclustered index Ix_Tc_aid on Tc(aid)

-- Nonclustered seek 
select *
from TestView 
