set deadlock_priority low

begin tran

update Packages
set name = 'dynasty2'
where id = 2

update Packages
set name = 'ghc2'
where id = 1

commit tran

select @@SPID
select @@TRANCOUNT
