set tran isolation level repeatable read
-- set tran isolation level serializable

begin tran
select * from Maintainers

waitfor delay '00:00:05'
select * from Maintainers

commit tran
