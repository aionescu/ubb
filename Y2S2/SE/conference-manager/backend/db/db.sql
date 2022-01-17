create table Users
  ( id bigserial primary key
  , email varchar(100) not null
  , password varchar(100) not null
  , role varchar(100) not null
  );

create table Listeners
  ( id bigserial primary key
  , name varchar(100) not null
  );

create table Chairs
  ( id bigserial primary key
  , name varchar(100) not null
  , email varchar(100) not null
  );

create table Authors
  ( id bigserial primary key
  , name varchar(100) not null
  , email varchar(100) not null
  , affiliation varchar(100) not null
  );

create table PcMembers
  ( id bigserial primary key
  , name varchar(100) not null
  , email varchar(100) not null
  , username varchar(100) not null
  , affiliation varchar(100) not null
  , webPage varchar(100) not null
  );

create table Admins
  ( id bigserial primary key
  , name varchar(100) not null
  , email varchar(100) not null
  );

create table Papers
  ( id bigserial primary key
  , name varchar(100) not null
  , topic varchar(100) not null
  , idAuthor bigint not null references Authors(id)
  );

create table Keywords
  ( id bigserial primary key
  , keyword varchar(50) not null
  );

create table PaperKeywords
  ( id bigserial primary key
  , idPaper bigint not null references Papers(id)
  , idKeyword bigint not null references Keywords(id)
  );

create table Grades
  ( id bigserial primary key
  , name varchar(50)
  );

create table PaperReviews
  ( id bigserial primary key
  , idPaper bigint not null references Papers(id)
  , idGrade bigint not null references Grades(id)
  , idReviewer bigint not null references PcMembers(id)
  );

create table Conferences
  ( id bigserial primary key
  , name varchar(100) not null
  , startDate date not null
  , endDate date not null
  , idAdmin bigint not null references Admins(id)
  );

create table Sections
  ( id bigserial primary key
  , idConference bigint not null references Conferences(id)
  , name varchar(100) not null
  );

create table SectionPapers
  ( id bigserial primary key
  , idSection bigint not null references Sections(id)
  , idPaper bigint not null references Papers(id)
  );
