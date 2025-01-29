
		-- here we are joining the 2 tables based on customer_id in both orders and customers tables
select *
from orders
join customers on orders.customer_id = customers.customer_id;

		-- here we can select any number of columns we want to see after joining.
        -- here if we directly specify customer_id then it will be ambiguous because it is present in both orders and customers tables
        -- so specify like orders.customer_id or customers.customer_id
select order_id, customers.customer_id, first_name, last_name
from orders
join customers on orders.customer_id = customers.customer_id;

		-- in the above commands we have repeated orders and customers multiple times but it's really verbose so we can specify that by aliasing the tables with shorter names or by simply characters specified like below
        -- but we don't need to use 'as' keyword for alias purpose
select order_id, o.customer_id, first_name, last_name
from orders o
join customers c
on o.customer_id = c.customer_id;

-- excersize
select order_id, oi.product_id, name, quantity, oi.unit_price
from order_items oi
join products p
on oi.product_id = p.product_id;

