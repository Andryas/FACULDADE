#!/bin/bash
cd

echo ------------------------------------------------------
echo "Movendo pasta e renomeando..."

# Move o arquivo de ~/Downloads para ~
mv ~/Downloads/MinTexJs-master ~

# Renomeia o arquivo
mv MinTexJs-master MinTexJs

echo "Permissões para scripts web-scraping"
cd ~/MinTexJs/Web-Scraping
echo "Permissão para arquivo P_ESTADAO.R"
chmod +x P_ESTADAO.R
echo "Permissão para arquivo P_OGLOBO.R" 
chmod +x P_OGLOBO.R
echo "Permissão para arquivo P_UOL.R"
echo ------------------------------------------------------
PESTADAO="  15 */5 * * * cd ~/MinTexJs/Web-Scraping && Rscript ./P_ESTADAO.R"
POGLOBO="  30 */5 * * * cd ~/MinTexJs/Web-Scraping && Rscript ./P_OGLOBO.R"
PUOL="  0 */12 * * * cd ~/MinTexJs/Web-Scraping && Rscript ./P_UOL.R"
crontab -l > scriptR
echo "Adiciona P_ESTADAO em crontab."
echo "$PESTADAO" >> scriptR
echo "Adiciona P_OGLOBO em crontab."
echo "$POGLOBO" >> scriptR
echo "Adiciona P_UOL em crontab."
echo "$PUOL" >> scriptR
crontab scriptR
rm scriptR

echo ------------------------------------------------------
if which R >/dev/null; then 
	echo "R já está instalado."
	echo "Verificando dependências"
	
	if dpkg -s libcurl4-openssl-dev >/dev/null ; then 
		echo "OK"
	else	
		sudo apt-get install -y libcurl4-openssl-dev
	fi	

	if dpkg -s libssl-dev >/dev/null ; then 
		echo "OK"	
	else
		sudo apt-get install -y libssl-dev
	fi	

	if dpkg -s libxml2-dev >/dev/null ; then 
		echo "OK"
	else
		sudo apt-get install -y libxml2-dev
	fi

	if dpkg -s libgsl-dev >/dev/null ; then 
		echo "OK"
	else		
		sudo apt-get install -y libgsl-dev
	fi	
	
	if dpkg -s libcairo2-dev >/dev/null ; then 
		echo "OK"
	else
		# topicmodels
		sudo apt-get install -y libcairo2-dev
	fi   
else
	echo "Instalando R..."
	sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
	sudo apt-get update
	sudo apt-get install r-base

	if dpkg -s libcurl4-openssl-dev >/dev/null ; then 
		echo "OK"
	else	
		sudo apt-get install -y libcurl4-openssl-dev
	fi	

	if dpkg -s libssl-dev >/dev/null ; then 
		echo "OK"	
	else
		sudo apt-get install -y libssl-dev
	fi	

	if dpkg -s libxml2-dev >/dev/null ; then 
		echo "OK"
	else
		sudo apt-get install -y libxml2-dev
	fi

	if dpkg -s libgsl-dev >/dev/null ; then 
		echo "OK"
	else		
		sudo apt-get install -y libgsl-dev
	fi	
	
	if dpkg -s libcairo2-dev >/dev/null ; then 
		echo "OK"
	else
		sudo apt-get install -y libcairo2-dev
	fi
	
fi
echo "Instala pacotes necessários no R"

R -e "packages <- c('shiny','mailR','shinyjs','devtools','XML','tm','SnowballC','topicmodels','ggplot2','plyr','wordcloud','reshape2') ; if (length(setdiff(packages, rownames(installed.packages()))) > 0) { install.packages(setdiff(packages, rownames(installed.packages())))}" 


echo ------------------------------------------------------
if which java >/dev/null; then
	echo "Java já está instalado."
else
	echo "Instalando Java Development Kit..."
	sudo apt-get update
	sudo apt-get install -y default-jdk
fi
echo ------------------------------------------------------
echo "Cria icone para a aplicação"

cat > mintexjs.desktop << EOL
[Desktop Entry]
Name=MinTexJs 1.0
Comment=New adviser
Exec=R -e "shiny::runApp('~/MinTexJs/App',port=7777,launch.browser =  TRUE,host = getOption('shiny.host','127.0.0.1'))"
Icon=/home/$USER/MinTexJs/icon.png
Terminal=true
Type=Application
EOL
	sudo mv mintexjs.desktop /usr/share/applications/



