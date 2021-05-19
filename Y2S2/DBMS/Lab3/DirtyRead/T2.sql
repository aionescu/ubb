set tran isolation level read uncommitted
-- set tran isolation level read committed

begin tran

select * from Maintainers

waitfor delay '00:00:15'
select * from Maintainers

commit tran
