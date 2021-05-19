set tran isolation level snapshot

begin tran

select * from Maintainers where id = 1

waitfor delay '00:00:10'
select * from Maintainers where id = 1

update Maintainers
set username = 'alex-ionescu'
where id = 1

commit tran
