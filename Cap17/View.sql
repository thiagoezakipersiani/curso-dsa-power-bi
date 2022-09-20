create or replace view erp."VW_FUNC"
as
select round(avg(f.salario_hora)), e.estado
from erp."TB_FUNC" f 
inner join  erp."TB_ENDERECO" e on e.id_func=f.id
group by e.estado;

ALTER TABLE erp."VW_FUNC"
	OWNER TO postgres;