create table if not exists Users
  ( id int primary key generated always as identity
  , username text unique not null
  , password text not null
  );

create table if not exists Files
  ( id int primary key generated always as identity
  , ownerID int not null
  , fileName text not null
  , fileType text not null
  , contents text not null
  )
