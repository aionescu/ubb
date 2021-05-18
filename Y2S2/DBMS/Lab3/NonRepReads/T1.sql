begin tran
waitfor delay '00:00:07'

update Maintainers
set username = 'ionescu'
where id = 1

commit tran
