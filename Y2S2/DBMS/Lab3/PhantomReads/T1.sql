begin tran
waitfor delay '00:00:05'

insert into Maintainers(username, email) values ('torvalds', 'torvalds@linux.org')
commit tran

delete from Maintainers where username = 'torvalds'
