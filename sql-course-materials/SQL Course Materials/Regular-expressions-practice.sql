-- select * from customers
-- where last_name like '%field';
		-- caret ^, to search in the starting 
-- where last_name REGEXP '^field';
		-- dollar $, to search in the ending
 -- where last_name REGEXP 'field$';
		-- | pipe character then it will search for field or mac or rose in the last_name column or last name should start with field
 -- where last_name regexp '^field|mac|rose';
 
		-- it will search for below combinations in last name
        -- ge
        -- ie
        -- me
 -- where last_name REGEXP '[gim]e';
		-- [a-h] specfies the range means a-h any characters can be present before the e in the last_name
 -- where last_name REGEXP '[a-h]e';
 
 -- execersize
 
 select * from customers
 #where first_name regexp 'elka|ambur';
 #where last_name regexp 'ey$|on$';
 #where last_name regexp '^my|se';
 where last_name regexp 'B[RU]';
 