# Controle de Processos Industriais

Disciplina ministrada pelo Professor Dr. [Fernando de Pol Mayer](http://leg.ufpr.br/~fernandomayer/)

# Objetivo do experimento

O objetivo do experimento é medir a velocidade com que os indivíduos leem, variando o estilo com que o texto digital é apresentado. Em outras palavras, encontrar a combinação entre as variáveis explicativas que minimiza o tempo de leitura.

# Conhecimento relevante sobre o problema

Pode-se dizer que não existe muita informação relevante sobre as variáveis envolvidas na leitura, é uma área ainda nova, principalmente quando se fala em medir a velocidade da leitura. Em geral, os estudos encontrados trabalham com a qualidade da leitura e concluem que o tamanho da fonte não interfere na absorção de conhecimento, mas o tipo de fonte sim. Fora isso, tem-se algumas informações em manuais como o LaTeX que possui toda uma estrutura bem definida, a qual se argumenta que é a melhor estrutura para uma boa leitura. Mas em geral, é muito subjetivo, pois cada pessoa tem um gosto peculiar para/com sua leitura. 

Sendo assim a proposta do trabalho é verificar se de fato isso é verdade ou se essa subjetividade tem algum padrão.
		
# Variável resposta

Neste tipo de experimento é comum utilizar tipos diferentes de variações no estilo do texto para mensurar a qualidade da leitura. Em geral esse procedimento é avaliado atráves de questões de multipla escolha. Contudo para esse experimento a variável resposta vai ser dada pelo **tempo de leitura**.

A forma como é medida a variável resposta se da atráves de uma aplicação em shiny, na qual o indivíduo preenche um ID (que é fornecido por um dos alunos integrantes do grupo) e aperta em um botão verde, que nesse caso é o 'start', então um texto aparece, e após a leitura o indivíduo clica no botão vermelho que nesse caso é o "stop". Assim o tempo de leitura é calculo atráves do momento stop menos o momento start. Esse processo se repete para os demais textos.

TL = Start - Stop

TL = Tempo de Leitura

# Variáveis explicativas (fatores controláveis)

Muitos fatores podem influenciar na leitura de uma pessoa. Mais ainda quando essa leitura é digital, pois estamos mais expostos a fatores distrativos.

As principais variáveis que podem ter efeito no tempo de leitura são: tamanho da fonte, tipo da fonte, espaçamento entre as linhas, quantidade de caracteres por linha, cor da fonte, interesse e conhecimento do indivíduo, luminosidade do aparelho, ambiente, poluição da página etc...

As covariáveis consideradas para este experimento são:

* tamanho da fonte (12px-16px)

* tipo da fonte (arial-serif)

* espaçamento das linhas (120%-180%)

* quantidade de caracteres por linha (60-75)

O tamanho e o tipo de fonte foram selecionados com base em duas matérias na internet que fazem referência ao mesmo estudo [1](http://blogexamedeordem.com.br/estudo-indica-que-a-fonte-da-letra-pode-ajudar-na-memorizacao-e-no-aprendizado) [2](https://www.terra.com.br/noticias/ciencia/pesquisa/estudo-sugere-que-estilo-da-fonte-pode-ajudar-no-aprendizado,aa69b801069ea310VgnCLD200000bbcceb0aRCRD.html). Quanto a quantidade de caracteres por linha, existem várias teórias quanto ao melhor número de caracteres. Para este experimento será considerado 60 caracteres vs 75 caracteres com base nessa [máteria ](https://baymard.com/blog/line-length-readability) que argumenta que o melhor comprimento da linha está entre 50-60 caracteres mas que alguns estudos sugerem que 75 caracteres também é um bom comprimento. E a escolha do espaçamento das linhas foi selecionada atráves da comparação de varios valores, e após certa discussão chegou-se ao consenso desses valores. 

Sobre os efeitos esperados, pelas máterias citadas no começo do parágrafo anterior, esperamos obter resultados semelhantes quanto ao tamanho da fonte e o estilo da fonte. Ou seja, para o tamanho esperamos que que o tempo de leitura seja mais rápido conforme a letra aumenta, até certo ponto. E para o estilo da fonte esperamos que tenha uma leitura mais lenta quando a fonte não é uma fonte usual. Para a quantidade de caracteres espera-se uma maior velocidade de leitura quando fixada em 60 caracteres, pois segundo a máteria que se faz referência ao tamanho ideal da linha, linhas com muitos caracteres tendem a quebrar o ritmo da leitura.

# Fatores mantidos constantes

Os fatores mantidos constantes são:

* Cor da fonte: Preto

* Poluição da página: Branca, sem propagandas e efeitos


A forma de controle é feita atráves da aplicação em shiny.

# Fatores nuisance

Os fatores considerados nuisances para este experimento são:

* Interesse

* Conhecimento prévio

* Ambiente

Para minimizar os possiveis efeitos advindos dessas fontes de variação, as pessoas serão consideradas blocos e instruidas a realizar o experimento na parte do dia que se sentirem mais confortaveis. 

# Interações

Espera-se que tamanho e estilo da fonte junto com a quantidade de caracteres por linha tenham uma interação tripla. Neste caso a combinação que se espera que minimize o tempo de leitura é estilo: Arial, tamanho: 16px e quantidade de caraceteres: 60.


# Restrições do experimento

Algumas dificuldades do experimento consistem principalmente no processo de execução. Pois para garantirmos resultados integros optou-se por uma amostra selecionada de pessoas próximas ao integrantes do grupo para a execução do experimento. Pessoas essas que garantimos que querem e executarão o experimento de forma integra.

# Desenho experimental inicial

Experimento fatorial em quadrado latino.

Combinações do experimento (Exemplo: set.seed(1234))

| pessoa | texto | trat               |
|--------|-------|--------------------|
| I      | text1 | serif;12px;120%;60 |
| II     | text1 | arial;12px;120%;60 |
| III    | text1 | serif;24px;120%;60 |
| IV     | text1 | arial;24px;120%;60 |
| I      | text2 | serif;12px;180%;60 |
| II     | text2 | arial;12px;180%;60 |
| III    | text2 | serif;24px;180%;60 |
| IV     | text2 | arial;24px;180%;60 |
| I      | text3 | serif;12px;120%;75 |
| II     | text3 | arial;12px;120%;75 |
| III    | text3 | serif;24px;120%;75 |
| IV     | text3 | arial;24px;120%;75 |
| I      | text4 | serif;12px;180%;75 |
| II     | text4 | arial;12px;180%;75 |
| III    | text4 | serif;24px;180%;75 |
| IV     | text4 | arial;24px;180%;75 |

Onde cada combinação é aplicado uma vez a uma das cazelas abaixo.

|     | txt1 | txt2 | txt3 | txt4 |
|-----|------|------|------|------|
| I   | 13   | 1    | 16   | 2    |
| II  | 8    | 11   | 12   | 4    |
| III | 10   | 14   | 7    | 9    |
| IV  | 3    | 15   | 5    | 6    |