select avg(idade), (numero_filhos), grau_instrucao from erp."TB_FUNC"
where reg_procedencia='capital'
and estado_civil='casado'
and salario_hora > (select avg(salario_hora) from erp."TB_FUNC")
group by grau_instrucao,(numero_filhos)
order by avg(idade) desc


