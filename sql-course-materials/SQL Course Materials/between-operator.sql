select * from customers
-- where points >= 1000 && points <= 3000;
-- between operator is used to specify the range between any two numbers and both are inclusive like >= and <=
where points between 1000 and 3000;


-- execersize
select * from customers
where birth_date between '1990-01-01' and '2000-01-01';