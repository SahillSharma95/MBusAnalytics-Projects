use EASYCARE;

-- Q1: How many of each Gardening Service type was provided over the 3 month period? 
select a.service_typeid as ID, count(a.service_typeid) as Count, b.service_type as Service
from volunteergarden as a
inner join services as b on a.service_typeid = b.service_id
group by a.service_typeid
;

-- Q2: How many volunteering jobs did each of the 13 volunteers perform over the 3 month period, and does this align with their entered availabilities?
select a.volunteer_id, count(a.volunteer_id) as Count, b.vol_firstname, b.vol_lastname, c.vol_availabilitydays
from volunteergarden as a
left join volunteer as b on a.volunteer_id = b.volunteer_id
left join availability as c on b.vol_availabilityid = c.vol_availabilityid
group by a.volunteer_id
order by count(a.volunteer_id) desc
;

-- Q3: How many volunteering jobs were performed in each of the 3 months?
select monthname(a.maintain_date) as Month, count(monthname(a.maintain_date)) as Count
from volunteergarden as a
group by monthname(a.maintain_date)
;


-- Q5: How many volunteering jobs did each address actually receive over the 3 month period?
select a.garden_id, count(a.garden_id) as Count, b.garden_addr
from volunteergarden as a
left join garden as b on a.garden_id = b.garden_id
group by a.garden_id
order by count(a.garden_id) desc
;

-- Q5: What is the average time a job is started, per month?
select monthname(a.maintain_date) as Month, count(monthname(a.maintain_date)) as Count, SEC_TO_TIME(AVG(TIME_TO_SEC(maintain_time))) as Average
from volunteergarden as a
group by monthname(a.maintain_date)
;


