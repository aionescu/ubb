set deadlock_priority high

begin tran

update Packages
set name = 'ghc1'
where id = 4

update Packages
set name = 'dynasty1'
where id = 5

commit tran

select @@SPID
select @@TRANCOUNT
