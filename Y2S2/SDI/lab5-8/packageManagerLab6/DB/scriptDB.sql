create table Package
(
	id bigserial primary key,
	name varchar(255) unique not null,
	description varchar(255) not null,
	sourceRepo varchar(255) not null,
	license varchar(255) not null
)

create table Maintainer
(
	id bigserial primary key,
	userName varchar(255) unique not null,
	fullName varchar(255) not null,
	email varchar(255) unique not null
)

create table PackageMaintainer
(
	id bigserial primary key ,
	packageID bigint not null references Package(id) on delete cascade,
	maintainerID bigint not null references Maintainer(ID) on delete cascade
)

create table PackageVersion(
	id bigserial primary key,
	packageID bigint not null references Package(id) on delete cascade,
	versionNumber varchar(255) not null
)

INSERT INTO Maintainer(username, fullname, email) VALUES
('m1', 'maintainer1', 'm1@gmail.com'),
('m2', 'john', 'john@gmail.com'),
('m3', 'thomas', 'thomas@yahoo.com'),
('m4', 'rock', 'rock@yahoo.com')

INSERT INTO Package(name, description, sourcerepo, license) VALUES
('p1','dasd das', 'https://source1.com', 'lcere'),
('p2', 'ufen feubfe', 'https://source2.com', 'lrei'),
('p3','dsan das', 'https://visit.com', 'tutip'), 
('p4', 'bn irolkw rkwn', 'https://story.com', 'mjio')

INSERT INTO PackageMaintainer(packageid, maintainerid) VALUES
(3,3), (3,1), (2,2), (2,1)

INSERT INTO PackageVersion(packageid, versionNumber) values
(1, '1.23.4') , (2, '2.635.8')