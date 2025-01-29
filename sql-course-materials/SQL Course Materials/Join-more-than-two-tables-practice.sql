use sql_store;

select 
	o.order_id, o.order_date, c.first_name, c.last_name, os.name as status
from orders o
join customers c
on o.customer_id = c.customer_id
join order_statuses os
on o.status = os.order_status_id;

use sql_invoicing;

select 
	p.payment_id,
    c.name as client_name,
    i.number as invoice_number,
    pm.name as payment_method
from payments p
join clients c
	on p.client_id = c.client_id
join invoices i
	on p.invoice_id = i.invoice_id
join payment_methods pm
	on p.payment_method = pm.payment_method_id
