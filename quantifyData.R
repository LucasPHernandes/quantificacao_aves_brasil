
# ANÁLISE DOS DADOS

# Junta as linhas das tabelas selecionadas (precisa haver correspondência de nomes de colunas)
tabela_analise <- bind_rows(SP, SC_2009, SC_2011, RS_2002, RS_2014, RJ, PR_2004, PR_2018, PA, MG, ES_2005, ES_2022, BA)
view(tabela_analise)
