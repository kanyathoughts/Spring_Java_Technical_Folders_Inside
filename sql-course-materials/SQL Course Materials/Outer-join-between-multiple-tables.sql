use sql_store;

select 
	c.customer_id,
	o.order_id,
    c.first_name,
    s.shipper_id as shipper_id,
    s.name as shipper_name
from customers c
left join orders o
	on c.customer_id = o.customer_id
left join shippers s
	on s.shipper_id = o.shipper_id;
    
-- execrcize
-- we can combine inner and outer joins
-- here all the orders are having customers so we can use inner join
-- like wise all the orders are having status so we can use inner join.
-- but for few orders only we have shippers so if we use inner join we will get the orders that having shippers but we want all the orders so we have to use left outer join over there.
use sql_store;

select
	o.order_date,
    o.order_id,
    c.first_name,
    sh.name as shipper_name,
    s.name as status
from orders o
join customers c
	on o.customer_id = c.customer_id
left join shippers sh
	on o.shipper_id = sh.shipper_id
join order_statuses s
	on o.status = s.order_status_id;