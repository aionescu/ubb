set deadlock_priority low

begin tran

update Packages
set name = 'dynasty2'
where id = 5

update Packages
set name = 'ghc2'
where id = 4

commit tran

select @@SPID
select @@TRANCOUNT
