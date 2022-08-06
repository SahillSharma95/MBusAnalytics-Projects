use easycare_v2;

CREATE TABLE VOLUNTEER (
VOLUNTEER_ID INT PRIMARY KEY,
VOL_FIRSTNAME CHAR (50),
VOL_LASTNAME CHAR (50),
VOL_DOB DATE,
VOL_MOBILENUM CHAR (50),
VOL_AVAILABILITYID INT
);

CREATE TABLE AVAILABILITY (
VOL_AVAILABILITYID INT PRIMARY KEY,
VOL_AVAILABILITYDAYS CHAR (50)
);

CREATE TABLE GARDEN (
GARDEN_ID INT PRIMARY KEY,
GARDEN_ADDR CHAR (50),
GARDEN_TYPEID INT,
RESI_FIRSTNAME CHAR(50),
RESI_LASTNAME CHAR(50),
RESI_DOB DATE
);

CREATE TABLE SERVICES (
SERVICE_ID INT PRIMARY KEY,
SERVICE_TYPE CHAR(50)
);

CREATE TABLE MAINTENANCEPERIOD (
PERIOD_ID INT PRIMARY KEY,
MAINT_PERIOD CHAR(50)
);

CREATE TABLE VOLUNTEERGARDEN (
VOLUNTEER_ID INT,
GARDEN_ID INT,
MAINTAIN_DATE DATE,
MAINTAIN_TIME TIME,
SERVICE_TYPEID INT,
PRIMARY KEY (VOLUNTEER_ID, GARDEN_ID)
);


-- ----------INSERTION OF VALUES INTO EACH CREATED TABLE--------------------

INSERT INTO VOLUNTEER VALUES (
1, 'Sahill', 'Sharma', '1995-05-31', '0424767331', 1
);

INSERT INTO VOLUNTEER VALUES (
2, 'David', 'Jones', '1992-02-17', '0408010621', 7
);

INSERT INTO VOLUNTEER VALUES (
3, 'Greg', 'Norman', '1985-05-01', '0422041313', 1
);

INSERT INTO VOLUNTEER VALUES (
4, 'Casio', 'Jones', '2002-04-03', '0496509626', 5
);

INSERT INTO VOLUNTEER VALUES (
5, 'Joe', 'Citizen', '2003-05-06', '0420178835', 7
);

INSERT INTO VOLUNTEER VALUES (
6, 'Jeff', 'Smith', '2005-12-05', '0471013573', 5
);

INSERT INTO VOLUNTEER VALUES (
7, 'Leslie', 'Jones', '2001-01-26', '0445570629', 5
);

INSERT INTO VOLUNTEER VALUES (
8, 'George', 'Bush', '2002-05-05', '0411040817', 3
);

INSERT INTO VOLUNTEER VALUES (
9, 'Bill', 'Clinton', '2000-05-15', '0471290695', 2
);

INSERT INTO VOLUNTEER VALUES (
10, 'Barack', 'Obama', '2000-06-20', '0435351237', 3
);

INSERT INTO VOLUNTEER VALUES (
11, 'John', 'Howard', '2000-10-14', '0428622520', 7
);

INSERT INTO VOLUNTEER VALUES (
12, 'Bill', 'Gates', '2003-08-08', '0449280853', 4
);

INSERT INTO VOLUNTEER VALUES (
13, 'Steve', 'Jobs', '2001-11-06', '0482822758', 6
);

INSERT INTO AVAILABILITY VALUES (
1, '1 day per week'
);

INSERT INTO AVAILABILITY VALUES (
2, '2 days per week'
);

INSERT INTO AVAILABILITY VALUES (
3, '3 days per week'
);

INSERT INTO AVAILABILITY VALUES (
4, '4 days per week'
);

INSERT INTO AVAILABILITY VALUES (
5, '5 days per week'
);

INSERT INTO AVAILABILITY VALUES (
6, '6 days per week'
);

INSERT INTO AVAILABILITY VALUES (
7, '7 days per week'
);

ALTER TABLE GARDEN DROP COLUMN GARDEN_TYPEID;

INSERT INTO GARDEN VALUES (
1, '1 John St', 'Doris', 'Lane', '1944-06-13'
);

INSERT INTO GARDEN VALUES (
2, '1 Adelaide St', 'Loren', 'Lane', '1941-02-14'
);

INSERT INTO GARDEN VALUES (
3, '1 Perth St', 'Ken', 'James', '1946-09-21'
);

INSERT INTO GARDEN VALUES (
4, '1 Darwin St', 'Virginia', 'Osborne', '1946-05-08'
);

INSERT INTO GARDEN VALUES (
5, '1 Hobart St', 'Jermaine', 'Figueroa', '1948-09-19'
);

INSERT INTO GARDEN VALUES (
6, '1 Canberra St', 'Henry', 'Harper', '1940-09-23'
);

INSERT INTO GARDEN VALUES (
7, '1 Melbourne St', 'Glenda', 'Wong', '1945-02-18'
);

INSERT INTO GARDEN VALUES (
8, '1 Sydney St', 'Keith', 'Webster', '1942-10-29'
);

INSERT INTO GARDEN VALUES (
9, '1 Launceston St', 'Eunice', 'Ball', '1949-11-16'
);

INSERT INTO GARDEN VALUES (
10, '1 Brisbane St', 'Juana', 'Delgado', '1947-04-11'
);

INSERT INTO GARDEN VALUES (
11, '1 Cairns St', 'Angela', 'Owen', '1944-12-25'
);

INSERT INTO GARDEN VALUES (
12, '1 Rockhampton St', 'Kenneth', 'Franklin', '1946-12-01'
);

INSERT INTO GARDEN VALUES (
13, '1 Alice St', 'Lois', 'Mcbride', '1945-07-08'
);

INSERT INTO GARDEN VALUES (
14, '1 Spring St', 'Carolyn', 'Taylor', '1943-12-19'
);

INSERT INTO GARDEN VALUES (
15, '1 Broome St', 'Julian', 'Nguyen', '1948-07-11'
);

INSERT INTO GARDEN VALUES (
16, '1 Ryde St', 'Abraham', 'Ramos', '1945-08-27'
);

INSERT INTO GARDEN VALUES (
17, '1 Homebush St', 'Josh', 'Matthews', '1947-12-27'
);

INSERT INTO GARDEN VALUES (
18, '1 Beach St', 'Julio', 'Conner', '1948-11-03'
);

INSERT INTO GARDEN VALUES (
19, '1 City St', 'Alexis', 'Hanson', '1947-05-06'
);

INSERT INTO GARDEN VALUES (
20, '1 Apple St', 'Julie', 'Wolfe', '1946-07-23'
);

ALTER TABLE GARDEN ADD COLUMN MAINT_PERIODID INT;

UPDATE GARDEN SET MAINT_PERIODID  = 7 WHERE GARDEN_ID = 1;
UPDATE GARDEN SET MAINT_PERIODID  = 2 WHERE GARDEN_ID = 2;
UPDATE GARDEN SET MAINT_PERIODID  = 2 WHERE GARDEN_ID = 3;
UPDATE GARDEN SET MAINT_PERIODID  = 4 WHERE GARDEN_ID = 4;
UPDATE GARDEN SET MAINT_PERIODID  = 7 WHERE GARDEN_ID = 5;
UPDATE GARDEN SET MAINT_PERIODID  = 7 WHERE GARDEN_ID = 6;
UPDATE GARDEN SET MAINT_PERIODID  = 3 WHERE GARDEN_ID = 7;
UPDATE GARDEN SET MAINT_PERIODID  = 6 WHERE GARDEN_ID = 8;
UPDATE GARDEN SET MAINT_PERIODID  = 4 WHERE GARDEN_ID = 9;
UPDATE GARDEN SET MAINT_PERIODID  = 3 WHERE GARDEN_ID = 10;
UPDATE GARDEN SET MAINT_PERIODID  = 3 WHERE GARDEN_ID = 11;
UPDATE GARDEN SET MAINT_PERIODID  = 3 WHERE GARDEN_ID = 12;
UPDATE GARDEN SET MAINT_PERIODID  = 2 WHERE GARDEN_ID = 13;
UPDATE GARDEN SET MAINT_PERIODID  = 6 WHERE GARDEN_ID = 14;
UPDATE GARDEN SET MAINT_PERIODID  = 6 WHERE GARDEN_ID = 15;
UPDATE GARDEN SET MAINT_PERIODID  = 5 WHERE GARDEN_ID = 16;
UPDATE GARDEN SET MAINT_PERIODID  = 3 WHERE GARDEN_ID = 17;
UPDATE GARDEN SET MAINT_PERIODID  = 1 WHERE GARDEN_ID = 18;
UPDATE GARDEN SET MAINT_PERIODID  = 3 WHERE GARDEN_ID = 19;
UPDATE GARDEN SET MAINT_PERIODID  = 5 WHERE GARDEN_ID = 20;

INSERT INTO SERVICES VALUES (
1, 'Mulching'
);

INSERT INTO SERVICES VALUES (
2, 'Weeding'
);

INSERT INTO SERVICES VALUES (
3, 'Lawn Mowing'
);

INSERT INTO SERVICES VALUES (
4, 'Planting'
);

INSERT INTO SERVICES VALUES (
5, 'Cleaning'
);

INSERT INTO SERVICES VALUES (
6, 'Horticulture'
);

INSERT INTO MAINTENANCEPERIOD VALUES (
1, 'One Time a Week'
);

INSERT INTO MAINTENANCEPERIOD VALUES (
2, 'Two Times a Week'
);

INSERT INTO MAINTENANCEPERIOD VALUES (
3, 'Three Times a Week'
);

INSERT INTO MAINTENANCEPERIOD VALUES (
4, 'Four Times a Week'
);

INSERT INTO MAINTENANCEPERIOD VALUES (
5, 'Five Times a Week'
);

INSERT INTO MAINTENANCEPERIOD VALUES (
6, 'Six Times a Week'
);

INSERT INTO MAINTENANCEPERIOD VALUES (
7, 'Seven Times a Week'
);

UPDATE MAINTENANCEPERIOD SET MAINT_PERIOD = 'Daily' WHERE PERIOD_ID = 7;

INSERT INTO VOLUNTEERGARDEN VALUES (
'7', '2', '2022-01-27', '08:00', '2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'13','11','2022-03-20','14:22','2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'6','6','2022-02-08','11:04','1'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'3','1','2022-03-31','10:36','4'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'6','3','2022-01-28','8:40','4'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'10','16','2022-02-22','11:15','4'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'9','10','2022-01-29','10:40','1'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'12','17','2022-01-14','11:14','5'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'10','14','2022-01-05','13:55','3'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'2','9','2022-03-07','9:23','3'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'4','8','2022-01-06','10:50','2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'10','15','2022-02-04','16:48','3'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'9','4','2022-01-23','12:18','2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'4','14','2022-03-30','13:41','5'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'6','17','2022-03-05','12:02','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'11','5','2022-02-28','12:51','4'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'13','19','2022-01-29','16:44','3'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'13','12','2022-03-11','8:52','2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'11','10','2022-02-27','14:34','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'2','11','2022-01-24','15:24','2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'11','20','2022-02-26','9:11','4'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'9','11','2022-03-22','13:32','5'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'4','13','2022-01-12','14:13','2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'1','17','2022-02-07','9:15','3'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'9','18','2022-02-06','14:47','3'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'13','4','2022-01-08','15:35','2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'13','13','2022-01-29','14:36','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'12','18','2022-03-11','13:08','4'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'3','4','2022-02-03','12:51','3'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'4','3','2022-03-24','11:31','1'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'9','13','2022-03-20','9:05','4'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'5','2','2022-02-02','11:41','3'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'9','8','2022-01-26','15:15','5'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'3','5','2022-03-22','8:58','4'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'11','9','2022-02-06','16:03','5'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'1','9','2022-03-07','16:02','5'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'1','20','2022-02-13','10:02','5'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'5','14','2022-03-08','14:35','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'11','7','2022-01-18','15:44','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'11','1','2022-01-04','10:23','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'7','19','2022-01-20','10:56','4'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'13','16','2022-03-02','14:06','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'7','16','2022-03-25','16:34','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'4','11','2022-02-17','8:31','2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'7','8','2022-02-03','13:10','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'3','20','2022-01-30','13:34','5'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'9','5','2022-01-30','9:35','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'9','19','2022-01-26','8:15','2'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'11','19','2022-02-09','16:00','6'
);

INSERT INTO VOLUNTEERGARDEN VALUES (
'1','7','2022-03-20','10:47','3'
);

SELECT * FROM VOLUNTEERGARDEN;

ALTER TABLE VOLUNTEERGARDEN ADD PRIMARY KEY (SERVICE_TYPEID);

DESCRIBE VOLUNTEERGARDEN;



SELECT * FROM VOLUNTEER;
SELECT * FROM GARDEN;
