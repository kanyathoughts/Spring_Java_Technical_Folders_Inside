use sql_store;

-- if we have 2 primary keys in one table and if we want to join that table with another table then we have to use and condtion inside the on condition to uniquely identify rows
select *
from order_items oi
join order_item_notes oin
	on oi.order_id = oin.order_Id
    and oi.product_id = oin.product_id;