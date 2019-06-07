<center><h1> Mineração de Texto em Jornais </h1></center>

***

<p style="font-size:16px" align="justify"> O objetivo desse repositório é armazenar os arquivos do trabalho proposto pela disciplina <a href="http://www.leg.ufpr.br/~walmes/ensino/mintex/">CE062 - Análise de Texto</a>, oferecida pela <i>Universidade Federal do Paraná</i> e ministrada pelo Prof.PhD <a href="http://www.leg.ufpr.br/doku.php/pessoais:walmes"> Walmes Marques Zeviani</a>. </p>


*** 

# Proposta do trabalho

<p align="justify">
O presente trabalho tem como propósito utilizar as técnicas aprendidas na disciplina para extrair informações pertinentes dos jornais. Devido nosso tempo ser finito e haver uma grande quantidade de matérias publicadas diariamente, fica praticamente impossivel nos mantermos informados sobre os acontecimentos e fatos diários. Sendo assim, o objetivo central é encontrar os <b>termos-chaves</b> dentro das notícias em um determinado período de tempo, criando assim um filtro o qual espera-se a diminuição de ruídos (ruído nesse contexto seria notícias que não agregam valor). O objetivo secundário é agrupar os <b>documentos</b> com base nos <b>termos-chaves</b> e então criar uma lista ponderada para as notícias. Ou seja, este programa visa identificar os temas relevantes do periodo de interesse e as notícias que melhor os representam, economizando tempo do leitor.
</p>

# Jornais

<p align="justify">
A primeira parte do trabalho foi decidir quais jornais utilizar. Com base na <a href="http://www.anj.org.br/maiores-jornais-do-brasil/">Associação Nacional de Jornais</a> optou-se pelo <a href="http://g1.globo.com/">G1<a>,<a href="http://www.folha.uol.com.br/">Folha de São Paulo</a> e <a href="www.estadao.com.br">Estadão</a>. Para uma primeira análise também foi escolhido somente uma categoria desses três jornais, a aba escolhida é <i>Política</i> ou <i>Poder</i>.
</p>

# Etapas do trabalho

Após a primeira parte, foi definido 4 etapas subsequentes, são elas:

i. Web Scraping  
ii. Modelagem de tópicos e agrupamento por similaridade  
iii. Reprodução - RShiny


##### i. Web Scraping

A parte do web-scraping nesse contexto é a extração diária das notícias que são armazenadas em um arquivo *.RData*.


##### ii. Modelagem de tópicos e agrupamento por similaridade

Para a modelagem dos tópicos foi utilizado o *Latent Dirichlet Allocation* (LDA).

##### iii. Reprodução - R Shiny

O RShiny resultante tem seis panéis.

i.   Painel 0 - Controles, onde o usúario irá definir os inputs.  
ii.  Painel 1 - Gráfico da proporção dos tópicos.  
iii. Painel 2 - Wordclouds dos tópicos com as palavras mais frequentes.  
iv.  Painel 3 - Lista decrescente dos títulos relacionados com o tópico selecionado.     
v.   Painel 4 - Matério selecionada no painel 3.  
vi.  Painel 5 - Email - serve para receber e-mail da execução dos scripts de *Web-Scraping*  
