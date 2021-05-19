set deadlock_priority high

begin tran

update Packages
set name = 'ghc1'
where id = 1

update Packages
set name = 'dynasty1'
where id = 2

commit tran

select @@SPID
select @@TRANCOUNT

-- update Packages
-- set name = 'ghc'
-- where id = 1

-- update Packages
-- set name = 'dynasty'
-- where id = 2
