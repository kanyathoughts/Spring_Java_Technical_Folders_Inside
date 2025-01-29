use sql_store;

-- here in inner join we are getting only the customers who have orders
-- but if we want all the customers even if they don't have orders then we can use left join and from should be applied on customers table
-- same if we have used orders table fro from clause then if we want all the customers then we have to use right join
-- and c.customer_id should be printed as all the customers we need from customers table and not from the orders table
-- 
select 
	c.customer_id,
	o.order_id,
    c.first_name
from customers c
left join orders o
	on c.customer_id = o.customer_id;
    
select 
	c.customer_id,
	o.order_id,
    c.first_name
from orders o
right join customers c
	on c.customer_id = o.customer_id;
    
use sql_store;

select 
	p.product_id,
    p.name,
    oi.quantity
from products p
left outer join order_items oi
on p.product_id = oi.product_id;
    
    
    
    
    
    
    
    
    