declare @id int, @name nvarchar(50)

declare c1 cursor for select id, name from Packages

open c1
fetch next from c1 into @id, @name

while @@FETCH_STATUS = 0
begin
  print cast (@id as nvarchar(5)) + ' ' + @name
  fetch next from c1 into @id, @name
end

close c1
deallocate c1
