# Conjuntos

set Ob;							# Conjunto de nós
set Ol within Ob cross Ob;  	# Conjunto de ramos
set Ot;       					# Conjunto de intervalos de tempo
set AC;       					# Conjuno de ACs
set BAT;      					# Conjunto de baterias
set PFV;      					# Conjunto de painéis fotovoltáicos
set Of = 1 .. 3 by 1;   		# Conjunto de fases

# Parâmetros do sistema

param dT := 15/60;         		# Detal t = 15 min / 60 minutos (1 hora)

param Vmino;      				# Tensão mínima do sistema
param Vmaxo;  					# Tensão máxima do sistema

param th1 := 10 * 3.1416/180; 	# Ângulo de desvio máximo negativo (10°)
param th2 := 5 * 3.1416/180; 	# Ângulo de desvio máximo positivo (5°)

param tha := 0;    				# Ângulo da fase a
param thb := -2.0944;   		# Ângulo da fase b (-120°)
param thc := 2.0944;   			# Ângulo da fase c (120°)

param neper := 2.71828;

# Parâmetros de carga

param fdem{Ot}; 					# Fator de demanda dos períodos de tempo
param fdem_artigo{Ot}; 				# Fator de demanda dos períodos de tempo
param tarifa_branca{Ot};			# Fator que multiplica o preco da energia de acordo com o horario (tarifa branca) [%]
param tarifa_artigo{Ot};			# Fator que multiplica o preco da energia de acordo com o horario (tarifa artigo Rider) [%]
param preco_energia := 0.36448; 	# Preco da energia R$/kWh

# Nós

param Tipo{Ob};     			# Tipo de barra 1: carga, 0: SE

param Nivel_b{Ob};  			# Nivel de tensão do nó
param Nivel_l{Ol};				# Identifica se o ramo l é primário (Nivel_l = 1), secundário (Nivel_l = 2), ou trafo (Nivel_l = 999)

param at{Ol};     				# Relação de transformação da linha

param PDa{Ob};     				# Potência Ativa de Demanda no nó i na fase A p.u.
param QDa{Ob};     				# Potência Reativa de Demanda no nó i na fase A p.u.
param PDb{Ob};     				# Potência Ativa de Demanda no nó i na fase B p.u.
param QDb{Ob};     				# Potência Reativa de Demanda no nó i na fase B p.u.
param PDc{Ob};     				# Potência Ativa de Demanda no nó i na fase C p.u.
param QDc{Ob};     				# Potência Reativa de Demanda no nó i na fase C p.u.

var PCa{Ob,Ot} >= 0;     		# Potência Ativa Total no nó i na fase A p.u.
var PCb{Ob,Ot} >= 0;     		# Potência Ativa Total no nó i na fase B p.u.
var PCc{Ob,Ot} >= 0;     		# Potência Ativa Total no nó i na fase C p.u.

var QCa{Ob,Ot} >= 0;     		# Potência Reativa Total no nó i na fase A p.u.
var QCb{Ob,Ot} >= 0;     		# Potência Reativa Total no nó i na fase B p.u.
var QCc{Ob,Ot} >= 0;     		# Potência Reativa Total no nó i na fase C p.u.

param Vnom{Ob};     			# Magnitude de tensão nominal do nó
param Vmin{Ob};     			# Magnitude de tensão mínima do nó
param Vmax{Ob};     			# Magnitude de tensão máxima do nó

param Vrae{Ob};     			# Estimação da tensão Fase A
param Viae{Ob};     			# Estimação da tensão Fase A
param Vrbe{Ob};     			# Estimação da tensão Fase B
param Vibe{Ob};     			# Estimação da tensão Fase B
param Vrce{Ob};     			# Estimação da tensão Fase C
param Vice{Ob};     			# Estimação da tensão Fase C

param alfa_a{Ob};   			# Coeficiente da carga ativa Fase A
param alfa_b{Ob};   			# Coeficiente da carga ativa Fase B
param alfa_c{Ob};   			# Coeficiente da carga ativa Fase C

param beta_a{Ob};   			# Coeficiente da carga reativa Fase A
param beta_b{Ob};   			# Coeficiente da carga reativa Fase B
param beta_c{Ob};   			# Coeficiente da carga reativa Fase C

# Ramos

param Raa{Ol};    				# Resistência no circuito na fase A p.u.
param Xaa{Ol};    				# Reatância no circuito na fase A p.u.
param Rbb{Ol};    				# Resistência no circuito na fase B p.u.
param Xbb{Ol};    				# Reatância no circuito na fase B p.u.
param Rcc{Ol};    				# Resistência no circuito na fase C p.u.
param Xcc{Ol};    				# Reatância no circuito na fase C p.u.
param Xab{Ol};    				# Reatância no circuito na fase AB p.u.
param Xbc{Ol};    				# Reatância no circuito na fase BC p.u.
param Xac{Ol};    				# Reatância no circuito na fase AC p.u.
param Rab{Ol};    				# Reatância no circuito na fase AB p.u.
param Rbc{Ol};    				# Reatância no circuito na fase BC p.u.
param Rac{Ol};    				# Reatância no circuito na fase AC p.u.

param Imax{Ol};   				# Magnitude máxima de corrente permitido pelo ramo

# Variáveis do estado

# Tensões

var Vra{Ob,Ot};    				# Parte real da tensão V[i,j] na fase A inicial
var Via{Ob,Ot};    				# Parte imaginaria da tensão  V[i,j] na fase A inicial
var Vrb{Ob,Ot};    				# Parte real da tensão V[i,j] na fase B inicial
var Vib{Ob,Ot};    				# Parte imaginaria da tensão  V[i,j] na fase B inicial
var Vrc{Ob,Ot};    				# Parte real da tensão V[i,j] na fase C inicial
var Vic{Ob,Ot};    				# Parte imaginaria da tensão  V[i,j] na fase C inicial

# Fluxos de Corrente
 
var Ira{Ol,Ot};    				# Parte real do fluxo de corrente I[i,j] na fase A inicial
var Iia{Ol,Ot};    				# Parte imaginaria do fluxo de corrente I[i,j] na fase A inicial
var Irb{Ol,Ot};    				# Parte real do fluxo de corrente I[i,j] na fase B inicial
var Iib{Ol,Ot};    				# Parte imaginaria do fluxo de corrente I[i,j] na fase B inicial
var Irc{Ol,Ot};    				# Parte real do fluxo de corrente I[i,j] na fase C inicial
var Iic{Ol,Ot};    				# Parte imaginaria do fluxo de corrente I[i,j] na fase C inicial
				
# Geração				
				
var ISra{Ob,Ot};   				# Corrente real gerada na subestação na fase A
var ISia{Ob,Ot};   				# Corrente imaginaria gerada na subestação na fase A
var ISrb{Ob,Ot};   				# Corrente real gerada na subestação na fase B
var ISib{Ob,Ot};   				# Corrente imaginaria gerada na subestação na fase B
var ISrc{Ob,Ot};   				# Corrente real gerada na subestação na fase C
var ISic{Ob,Ot};   				# Corrente imaginaria gerada na subestação na fase C

# Parâmetros do ambiente

param Tout{Ot};							# Temperatura externa
param Irradiacao_sem_nuvem{Ot};			# Irradiacao solar [kW/m2]

# Modelo Térmico

var Tin{AC,Ot,Of};						# Temperatura interna da casa
var Tparede{AC,Ot,Of};					# Temperatura da parede

var Taux{AC,Ot,Of};						# Variáveis auxiliares para cálculo do valor absoluto do "conforto térmico dos usuários"
var Taux1{AC,Ot,Of} >= 0;
var Taux2{AC,Ot,Of} >= 0;

var Pac{AC,Ot,Of} >= 0;					# Potência ativa do AC [kW]
var Qac{AC,Ot,Of} >= 0;					# Potência reativa do AC [kVAr]
var Hac{AC,Ot,Of} >= 0;					# Capacidade de refrigeração do AC [kW]

var on_off{AC,Ot,Of} binary;			# Variável que determina se o AC está ligado ou desligado
var frequency_ac{AC,Ot,Of} >= 0;		# Potência de refrigeração do AC [kW]

param deltaT = 2;

###################################################
###################################################

# APARELHOS DE AR CONDICIONADO

param Tset_casa{AC};		# Temperature Setpoint

var var_a{AC};
var var_b{AC};

param Ra{AC};				# Resistência térmica da casa
param Ca{AC};				# Capacidade térmica da casa
param Rm{AC};				# Resistência térmica da parede
param Cm{AC};				# Capacidade térmica da parede

param Pnom_ac{AC};			# Potência nominal do aparelho de AC
param COP_nom{AC};			# Coeficiente de performance nominal do aparelho de AC

var desconforto{AC,Ot,Of};	# Medida de desconforto térmico do usuário

# Modificadores Pac

var mod_Pac_freq{AC,Ot,Of};	# Modificador de performance dos aparelhos de AC (Pac(freq))
var mod_Pac_Tout{Ot};		# Modificador de performance dos aparelhos de AC (Pac(Tout))	

# Modificadores Hac

var mod_Hac_freq{AC,Ot,Of};	# Modificador de performance dos aparelhos de AC (Hac(freq))	
var mod_Hac_Tout{Ot};		# Modificador de performance dos aparelhos de AC (Hac(Tout))
   
var Iac_re_a{AC,Ot};   				# Corrente real do AC da fase A
var Iac_re_b{AC,Ot};   				# Corrente real do AC da fase B
var Iac_re_c{AC,Ot};   				# Corrente real do AC da fase C
				
var Iac_im_a{AC,Ot};   				# Corrente imag do AC da fase A
var Iac_im_b{AC,Ot};   				# Corrente imag do AC da fase B
var Iac_im_c{AC,Ot};   				# Corrente imag do AC da fase C
				
param AC_Fase_a{AC};   				# Determina operação do AC na Fase A
param AC_Fase_b{AC};   				# Determina operação do AC na Fase B
param AC_Fase_c{AC};   				# Determina operação do AC na Fase C

# BATERIAS

param potencia_nom_bat_max{BAT}; 			# Potência de carragamento e descarregamento NOMINAIS da bateria [kW/kVAr]
param potencia_nom_bat_min{BAT}; 			# Potência de carragamento e descarregamento NOMINAIS da bateria [kW/kVAr]
param capacidade_bat{BAT};  			# Capacidade NOMINAL da bateria [kWh]

param eficiencia_carregamento := 0.95; 		# Eficiência de carregamento NOMINAL da bateria
param eficiencia_descarregamento := 0.95; 	# Eficiência de descarregamento NOMINAL da bateria

param BAT_Fase_a{BAT};  			# Determina operação das BAT da Fase A
param BAT_Fase_b{BAT};  			# Determina operação das BAT da Fase B
param BAT_Fase_c{BAT};  			# Determina operação das BAT da Fase C

var carga_bateria{BAT,Ot,Of} >= 0; 	# Carga da bateria em determinado intervalo de tempo [kWh]

var Pbateria_carga{BAT,Ot,Of};  		# Potência ativa da bateria (+/-)
var Pbateria_descarga{BAT,Ot,Of};  		# Potência reativa da bateria (+/0)
var Qbateria_descarga{BAT,Ot,Of};  		# Potência reativa da bateria (+/0)

param Sbateria := 5.8;
			
var Ibat_re_a{BAT,Ot};  			# Corrente real das BAT da Fase A
var Ibat_re_b{BAT,Ot};  			# Corrente real das BAT da Fase B
var Ibat_re_c{BAT,Ot};  			# Corrente real das BAT da Fase C
			
var Ibat_im_a{BAT,Ot};  			# Corrente imag das BAT da Fase A
var Ibat_im_b{BAT,Ot};  			# Corrente imag das BAT da Fase B
var Ibat_im_c{BAT,Ot};  			# Corrente imag das BAT da Fase C   

var Taux_bat1{BAT,Ot,Of} binary;
var Taux_bat2{BAT,Ot,Of};

# SISTEMAS FOTOVOLTAICOS

param eficiencia_pfv{PFV};  		# Eficiência do painel
param num_placas_pfv{PFV};  		# Número de placas solares
param area_pfv{PFV};				# Área dos painéis fotovoltaicos

var pot_pfv{PFV,Ot,Of};

param PFV_Fase_a{PFV};  			# Determina operação dos PFV da Fase A
param PFV_Fase_b{PFV};  			# Determina operação dos PFV da Fase B
param PFV_Fase_c{PFV};  			# Determina operação dos PFV da Fase C
    
var Ipfv_re_a{PFV,Ot}; 				# Corrente real dos PFV da Fase A
var Ipfv_re_b{PFV,Ot}; 				# Corrente real dos PFV da Fase B
var Ipfv_re_c{PFV,Ot}; 				# Corrente real dos PFV da Fase C
				
var Ipfv_im_a{PFV,Ot}; 				# Corrente imag dos PFV da Fase A
var Ipfv_im_b{PFV,Ot}; 				# Corrente imag dos PFV da Fase B
var Ipfv_im_c{PFV,Ot}; 				# Corrente imag dos PFV da Fase C   

#####################################################      
    
# Linearização dos Fluxos de Corrente

var Isqra{Ol,Ot} >= 0;  			# Quadrado das correntes da Fase A
var Isqrb{Ol,Ot} >= 0;  			# Quadrado das correntes da Fase B
var Isqrc{Ol,Ot} >= 0;  			# Quadrado das correntes da Fase C

param lambda;     					# Quantidade de intervalos em que a linearização da corrente é dividida
param m{Ol,1..lambda};
param delmax{Ol};    				# Variável auxiliar para o limite máximo do passo da corrente

var delra{Ol,Ot,1..lambda}>=0;
var delia{Ol,Ot,1..lambda}>=0;
var Irap{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos
var Iram{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos
var Iiap{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos
var Iiam{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos

var delrb{Ol,Ot,1..lambda}>=0;
var delib{Ol,Ot,1..lambda}>=0;
var Irbp{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos
var Irbm{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos
var Iibp{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos
var Iibm{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos

var delrc{Ol,Ot,1..lambda}>=0;
var delic{Ol,Ot,1..lambda}>=0;
var Ircp{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos
var Ircm{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos
var Iicp{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos
var Iicm{Ol,Ot}>=0;    				# Variáveis auxiliares para o cálculo da corrente nos ramos

# Variávies de Demanda

var IDra{Ob,Ot};	# Corrente real demandada na subestação na fase A inicial
var IDia{Ob,Ot};	# Corrente imaginaria demandada na subestação na fase A inicial
var IDrb{Ob,Ot};	# Corrente real demandada na subestação na fase B inicial
var IDib{Ob,Ot};	# Corrente imaginaria demandada na subestação na fase B inicial
var IDrc{Ob,Ot};	# Corrente real demandada na subestação na fase C inicial
var IDic{Ob,Ot};	# Corrente imaginaria demandada na subestação na fase C inicial

#--------------------------------------------- Funções Objetivo --------------------------------------

	# Desconforto

		minimize fo_desconforto: 
			(sum {n in AC, t in Ot, f in Of} (desconforto[n,t,f]))/card(AC)/card(Ot);

	# Consumo de energia

		minimize fo_consumo_energia_ativa_com_tarifa:
			sum {i in Ob, t in Ot : Tipo[i] == 1} (dT * tarifa_branca[t] * preco_energia) *
			(ISra[i,t] * Vra[i,t] + ISrb[i,t] * Vrb[i,t] + ISrc[i,t] * Vrc[i,t] +
			ISia[i,t] * Via[i,t] + ISib[i,t] * Vib[i,t] + ISic[i,t] * Vic[i,t]);
			
		minimize fo_consumo_energia_ativa_sem_tarifa:
			sum {i in Ob, t in Ot : Tipo[i] == 1} (dT * preco_energia) *
			(ISra[i,t] * Vra[i,t] + ISrb[i,t] * Vrb[i,t] + ISrc[i,t] * Vrc[i,t] +
			ISia[i,t] * Via[i,t] + ISib[i,t] * Vib[i,t] + ISic[i,t] * Vic[i,t]);

		minimize fo_consumo_energia_reativa_com_tarifa:
			sum {i in Ob, t in Ot : Tipo[i] == 1} (dT * tarifa_branca[t] * preco_energia) *
			(- ISia[i,t] * Vra[i,t] - ISib[i,t] * Vrb[i,t] - ISic[i,t] * Vrc[i,t] +
			 ISra[i,t] * Via[i,t] + ISrb[i,t] * Vib[i,t] + ISrc[i,t] * Vic[i,t]);
			 
		minimize fo_consumo_energia_reativa_sem_tarifa:
			sum {i in Ob, t in Ot : Tipo[i] == 1} (dT * preco_energia) *
			(- ISia[i,t] * Vra[i,t] - ISib[i,t] * Vrb[i,t] - ISic[i,t] * Vrc[i,t] +
			 ISra[i,t] * Via[i,t] + ISrb[i,t] * Vib[i,t] + ISrc[i,t] * Vic[i,t]);

		minimize fo_consumo_energia_com_tarifa:
			sum {i in Ob, t in Ot : Tipo[i] == 1} (dT * tarifa_branca[t] * preco_energia) *
			(ISra[i,t] * Vra[i,t] + ISrb[i,t] * Vrb[i,t] + ISrc[i,t] * Vrc[i,t] +
			ISia[i,t] * Via[i,t] + ISib[i,t] * Vib[i,t] + ISic[i,t] * Vic[i,t]) +

			sum {i in Ob, t in Ot : Tipo[i] == 1} (dT * tarifa_branca[t] * preco_energia) *
			(- ISia[i,t] * Vra[i,t] - ISib[i,t] * Vrb[i,t] - ISic[i,t] * Vrc[i,t] +
			ISra[i,t] * Via[i,t] + ISrb[i,t] * Vib[i,t] + ISrc[i,t] * Vic[i,t]);
			
		minimize fo_consumo_energia_sem_tarifa:
			sum {i in Ob, t in Ot : Tipo[i] == 1} (dT * preco_energia) *
			(ISra[i,t] * Vra[i,t] + ISrb[i,t] * Vrb[i,t] + ISrc[i,t] * Vrc[i,t] +
			ISia[i,t] * Via[i,t] + ISib[i,t] * Vib[i,t] + ISic[i,t] * Vic[i,t]) +

			sum {i in Ob, t in Ot : Tipo[i] == 1} (dT * preco_energia) *
			(- ISia[i,t] * Vra[i,t] - ISib[i,t] * Vrb[i,t] - ISic[i,t] * Vrc[i,t] +
			ISra[i,t] * Via[i,t] + ISrb[i,t] * Vib[i,t] + ISrc[i,t] * Vic[i,t]);

		minimize fo_multiobjetivo:
			sum {i in Ob, t in Ot : Tipo[i] == 1} (dT * tarifa_branca[t] * preco_energia) *
			(ISra[i,t] * Vra[i,t] + ISrb[i,t] * Vrb[i,t] + ISrc[i,t] * Vrc[i,t] +
			ISia[i,t] * Via[i,t] + ISib[i,t] * Vib[i,t] + ISic[i,t] * Vic[i,t])
			+ sum {n in AC, t in Ot, f in Of} << 2 ; 0 , 100000 >> desconforto[n,t,f];
			
	# Consumo de energia dos aparelhos de AC
	
		minimize fo_consumo_ac_com_tarifa:
			(sum {w in AC, t in Ot, f in Of} Pac[w,t,f] * dT * tarifa_branca[t] * preco_energia);
		
		minimize fo_consumo_ac_sem_tarifa:
			(sum {w in AC, t in Ot, f in Of} Pac[w,t,f] * dT);
	
	# Geração de energia dos PFVs
	
		minimize fo_consumo_pfv_com_tarifa:
			sum {p in PFV, t in Ot, f in Of} pot_pfv[p,t,f] * dT * tarifa_branca[t] * preco_energia;
		
		minimize fo_consumo_pfv_sem_tarifa:
			sum {p in PFV, t in Ot, f in Of} pot_pfv[p,t,f] * dT;
	
	# Consumo de energia das Baterias
	
		minimize fo_consumo_bat_com_tarifa:
			sum {b in BAT, t in Ot, f in Of} (Pbateria_carga[b,t,f] + Pbateria_descarga[b,t,f]) * dT * tarifa_branca[t] * preco_energia;
		
		minimize fo_consumo_bat_sem_tarifa:
			sum {b in BAT, t in Ot, f in Of} (Pbateria_carga[b,t,f] + Pbateria_descarga[b,t,f]) * dT;
	
		minimize fo_consumo_reativa_bat_com_tarifa:
			sum {b in BAT, t in Ot, f in Of} (Qbateria_descarga[b,t,f]) * dT * tarifa_branca[t] * preco_energia;
		
		minimize fo_consumo_reativa_bat_sem_tarifa:
			sum {b in BAT, t in Ot, f in Of} (Qbateria_descarga[b,t,f]) * dT;
	
	# Consumo de energia dos Aparelhos
	
		minimize fo_consumo_aparelhos_com_tarifa:
			sum {w in AC, t in Ot, f in Of}(Pac[w,t,f] + Pbateria_carga[w,t,f] + Pbateria_descarga[w,t,f] + pot_pfv[w,t,f]) * dT * tarifa_branca[t] * preco_energia;
		
		minimize fo_consumo_aparelhos_sem_tarifa:
			sum {w in AC, t in Ot, f in Of}(Pac[w,t,f] + Pbateria_carga[w,t,f] + Pbateria_descarga[w,t,f] + pot_pfv[w,t,f]) * dT;
		
	# Índice de demanda
	
		minimize fo_correntes_ao_quadrado: sum {(j,i) in Ol, t in Ot : Nivel_l[j,i] == 1} dT * tarifa_branca[t] * preco_energia * 
				(Ira[j,i,t]^2 + Iia[j,i,t]^2 + Irb[j,i,t]^2 + Iib[j,i,t]^2 + Irc[j,i,t]^2 + Iic[j,i,t]^2);	
	
#------------------------------------- Balanço de fluxos de correntes ------------------------

	# Balanço de corrente da fase A
	
		subject to corrente_balanco_real_a {i in Ob, t in Ot}:
			sum {(j,i) in Ol}(Ira[j,i,t]) - sum {(i,j) in Ol}((1/at[i,j])*Ira[i,j,t]) + 
			ISra[i,t] = IDra[i,t] 
			+ sum{n in AC  :  AC_Fase_a[n] == 1 && n == i}(Iac_re_a[n,t])      # Componente de corrente real do AC
			+ sum{n in BAT : BAT_Fase_a[n] == 1 && n == i}(Ibat_re_a[n,t])     # Componente de corrente real da BAT
			+ sum{n in PFV : PFV_Fase_a[n] == 1 && n == i}(Ipfv_re_a[n,t]);    # Componente de corrente real da PFV

		subject to corrente_balanco_imag_a {i in Ob, t in Ot}:
		 	sum {(j,i) in Ol}(Iia[j,i,t]) - sum {(i,j) in Ol}((1/at[i,j])*Iia[i,j,t]) + 
		 	ISia[i,t] = IDia[i,t]
		 	+ sum{n in AC  :  AC_Fase_a[n] == 1 && n == i}(Iac_im_a[n,t])      # Componente de corrente imag do AC
		 	+ sum{n in BAT : BAT_Fase_a[n] == 1 && n == i}(Ibat_im_a[n,t])     # Componente de corrente imag da BAT
		 	+ sum{n in PFV : PFV_Fase_a[n] == 1 && n == i}(Ipfv_im_a[n,t]);    # Componente de corrente imag da PFV

	# Balanço de corrente da fase B

		subject to corrente_balanco_real_b {i in Ob, t in Ot}:
		 	sum {(j,i) in Ol}(Irb[j,i,t]) - sum {(i,j) in Ol}((1/at[i,j])*Irb[i,j,t]) + 
		 	ISrb[i,t] = IDrb[i,t]
		 	+ sum{n in AC  :  AC_Fase_b[n] == 1 && n == i}(Iac_re_b[n,t])      # Componente de corrente real do AC
		 	+ sum{n in BAT : BAT_Fase_b[n] == 1 && n == i}(Ibat_re_b[n,t])     # Componente de corrente real da BAT
		 	+ sum{n in PFV : PFV_Fase_b[n] == 1 && n == i}(Ipfv_re_b[n,t]);    # Componente de corrente real da PFV

		subject to corrente_balanco_imag_b {i in Ob, t in Ot}:
			sum {(j,i) in Ol}(Iib[j,i,t]) - sum {(i,j) in Ol}((1/at[i,j])*Iib[i,j,t]) +
			ISib[i,t] = IDib[i,t]
			+ sum{n in AC  :  AC_Fase_b[n] == 1 && n == i}(Iac_im_b[n,t])      # Componente de corrente imag do AC
			+ sum{n in BAT : BAT_Fase_b[n] == 1 && n == i}(Ibat_im_b[n,t])     # Componente de corrente imag da BAT
			+ sum{n in PFV : PFV_Fase_b[n] == 1 && n == i}(Ipfv_im_b[n,t]);    # Componente de corrente imag da PFV
	
	# Balanço de corrente da fase C

		subject to corrente_balanco_real_c {i in Ob, t in Ot}:
			sum {(j,i) in Ol}(Irc[j,i,t]) - sum {(i,j) in Ol}((1/at[i,j])*Irc[i,j,t]) + 
			ISrc[i,t] = IDrc[i,t]
			+ sum{n in AC  :  AC_Fase_c[n] == 1 && n == i}(Iac_re_c[n,t])      # Componente de corrente real do AC
			+ sum{n in BAT : BAT_Fase_c[n] == 1 && n == i}(Ibat_re_c[n,t])     # Componente de corrente real da BAT
			+ sum{n in PFV : PFV_Fase_c[n] == 1 && n == i}(Ipfv_re_c[n,t]);    # Componente de corrente real da PFV

		subject to corrente_balanco_imag_c {i in Ob, t in Ot}:
		 	sum {(j,i) in Ol}(Iic[j,i,t]) - sum {(i,j) in Ol}((1/at[i,j])*Iic[i,j,t]) + 
		 	ISic[i,t] = IDic[i,t]
		 	+ sum{n in AC  :  AC_Fase_c[n] == 1 && n == i}(Iac_im_c[n,t])      # Componente de corrente imag do AC
		 	+ sum{n in BAT : BAT_Fase_c[n] == 1 && n == i}(Ibat_im_c[n,t])     # Componente de corrente imag da BAT
		 	+ sum{n in PFV : PFV_Fase_c[n] == 1 && n == i}(Ipfv_im_c[n,t]);    # Componente de corrente imag da PFV

#--------------------------------------------- Queda de tensão ----------------------------------------

	subject to queda_tensao_real_a {(i,j) in Ol, t in Ot}:
	 	Vra[i,t]-(at[i,j])*Vra[j,t]=Raa[i,j]*Ira[i,j,t]-Xaa[i,j]*Iia[i,j,t]+Rab[i,j]*Irb[i,j,t]-Xab[i,j]*Iib[i,j,t]+Rac[i,j]*Irc[i,j,t]-Xac[i,j]*Iic[i,j,t];

	subject to queda_tensao_imag_a {(i,j) in Ol, t in Ot}:
	 	Via[i,t]-(at[i,j])*Via[j,t]=Xaa[i,j]*Ira[i,j,t]+Raa[i,j]*Iia[i,j,t]+Xab[i,j]*Irb[i,j,t]+Rab[i,j]*Iib[i,j,t]+Xac[i,j]*Irc[i,j,t]+Rac[i,j]*Iic[i,j,t];

	subject to queda_tensao_real_b {(i,j) in Ol, t in Ot}:
		 Vrb[i,t]-(at[i,j])*Vrb[j,t]=Rbb[i,j]*Irb[i,j,t]-Xbb[i,j]*Iib[i,j,t]+Rab[i,j]*Ira[i,j,t]-Xab[i,j]*Iia[i,j,t]+Rbc[i,j]*Irc[i,j,t]-Xbc[i,j]*Iic[i,j,t];

	subject to queda_tensao_imag_b {(i,j) in Ol, t in Ot}:
	 	Vib[i,t]-(at[i,j])*Vib[j,t]=Xbb[i,j]*Irb[i,j,t]+Rbb[i,j]*Iib[i,j,t]+Xab[i,j]*Ira[i,j,t]+Rab[i,j]*Iia[i,j,t]+Xbc[i,j]*Irc[i,j,t]+Rbc[i,j]*Iic[i,j,t];

	subject to queda_tensao_real_c {(i,j) in Ol, t in Ot}:
		 Vrc[i,t]-(at[i,j])*Vrc[j,t]=Rcc[i,j]*Irc[i,j,t]-Xcc[i,j]*Iic[i,j,t]+Rac[i,j]*Ira[i,j,t]-Xac[i,j]*Iia[i,j,t]+Rbc[i,j]*Irb[i,j,t]-Xbc[i,j]*Iib[i,j,t];

	subject to queda_tensao_imag_c {(i,j) in Ol, t in Ot}:
	 	Vic[i,t]-(at[i,j])*Vic[j,t]=Xcc[i,j]*Irc[i,j,t]+Rcc[i,j]*Iic[i,j,t]+Xac[i,j]*Ira[i,j,t]+Rac[i,j]*Iia[i,j,t]+Xbc[i,j]*Irb[i,j,t]+Rbc[i,j]*Iib[i,j,t];

# ------------------ Demanda de potência ativa e reativa dependente da tensão (Linearização) ----------

	# Calculo da corrente de carga na fase a
		subject to corrente_carga_real_a {i in Ob, t in Ot}:
		 	IDra[i,t] = fdem_artigo[t]*((PDa[i]/(Vnom[i]^alfa_a[i])*Vrae[i])*((Vrae[i]^2 + Viae[i]^2)^(alfa_a[i]/2-1)) + (QDa[i]/(Vnom[i]^beta_a[i])*Viae[i])*((Vrae[i]^2 + Viae[i]^2)^(beta_a[i]/2-1))+
		    (PDa[i]/(Vnom[i]^alfa_a[i])*((Vrae[i]^2+Viae[i]^2)-2*Vrae[i]^2*(1-alfa_a[i]/2))*((Vrae[i]^2+Viae[i]^2)^(alfa_a[i]/2-2)) + QDa[i]/(Vnom[i]^beta_a[i])*(-2*(1-beta_a[i]/2)*Vrae[i]*Viae[i])*((Vrae[i]^2+Viae[i]^2)^(beta_a[i]/2-2)))*(Vra[i,t]-Vrae[i]) +
		    (QDa[i]/(Vnom[i]^beta_a[i])*((Vrae[i]^2+Viae[i]^2)-2*Viae[i]^2*(1-beta_a[i]/2))*((Vrae[i]^2+Viae[i]^2)^(beta_a[i]/2-2)) + PDa[i]/(Vnom[i]^alfa_a[i])*(-2*(1-alfa_a[i]/2)*Vrae[i]*Viae[i])*((Vrae[i]^2+Viae[i]^2)^(alfa_a[i]/2-2)))*(Via[i,t]-Viae[i]));

		subject to corrente_carga_imag_a {i in Ob, t in Ot}:
		 	IDia[i,t] = fdem_artigo[t]*((PDa[i]/(Vnom[i]^alfa_a[i])*Viae[i])*((Vrae[i]^2 + Viae[i]^2)^(alfa_a[i]/2-1)) - (QDa[i]/(Vnom[i]^beta_a[i])*Vrae[i])*((Vrae[i]^2 + Viae[i]^2)^(beta_a[i]/2-1))+
		    (-QDa[i]/(Vnom[i]^beta_a[i])*((Vrae[i]^2+Viae[i]^2)-2*Vrae[i]^2*(1-beta_a[i]/2))*((Vrae[i]^2+Viae[i]^2)^(beta_a[i]/2-2)) + PDa[i]/(Vnom[i]^alfa_a[i])*(-2*(1-alfa_a[i]/2)*Vrae[i]*Viae[i])*((Vrae[i]^2+Viae[i]^2)^(alfa_a[i]/2-2)))*(Vra[i,t]-Vrae[i]) +
		    ( PDa[i]/(Vnom[i]^alfa_a[i])*((Vrae[i]^2+Viae[i]^2)-2*Viae[i]^2*(1-alfa_a[i]/2))*((Vrae[i]^2+Viae[i]^2)^(alfa_a[i]/2-2)) - QDa[i]/(Vnom[i]^beta_a[i])*(-2*(1-beta_a[i]/2)*Vrae[i]*Viae[i])*((Vrae[i]^2+Viae[i]^2)^(beta_a[i]/2-2)))*(Via[i,t]-Viae[i]));

	# Calculo da corrente de carga na fase b
		subject to corrente_carga_real_b {i in Ob, t in Ot}:
		 	IDrb[i,t] = fdem_artigo[t]*((PDb[i]/(Vnom[i]^alfa_b[i])*Vrbe[i])*((Vrbe[i]^2 + Vibe[i]^2)^(alfa_b[i]/2-1)) + (QDb[i]/(Vnom[i]^beta_b[i])*Vibe[i])*((Vrbe[i]^2 + Vibe[i]^2)^(beta_b[i]/2-1))+
		    (PDb[i]/(Vnom[i]^alfa_b[i])*((Vrbe[i]^2+Vibe[i]^2)-2*Vrbe[i]^2*(1-alfa_b[i]/2))*((Vrbe[i]^2+Vibe[i]^2)^(alfa_b[i]/2-2)) + QDb[i]/(Vnom[i]^beta_b[i])*(-2*(1-beta_b[i]/2)*Vrbe[i]*Vibe[i])*((Vrbe[i]^2+Vibe[i]^2)^(beta_b[i]/2-2)))*(Vrb[i,t]-Vrbe[i]) +
		    (QDb[i]/(Vnom[i]^beta_b[i])*((Vrbe[i]^2+Vibe[i]^2)-2*Vibe[i]^2*(1-beta_b[i]/2))*((Vrbe[i]^2+Vibe[i]^2)^(beta_b[i]/2-2)) + PDb[i]/(Vnom[i]^alfa_b[i])*(-2*(1-alfa_b[i]/2)*Vrbe[i]*Vibe[i])*((Vrbe[i]^2+Vibe[i]^2)^(alfa_b[i]/2-2)))*(Vib[i,t]-Vibe[i]));

		subject to corrente_carga_imag_b {i in Ob, t in Ot}:
		 	IDib[i,t] = fdem_artigo[t]*((PDb[i]/(Vnom[i]^alfa_b[i])*Vibe[i])*((Vrbe[i]^2 + Vibe[i]^2)^(alfa_b[i]/2-1)) - (QDb[i]/(Vnom[i]^beta_b[i])*Vrbe[i])*((Vrbe[i]^2 + Vibe[i]^2)^(beta_b[i]/2-1))+
		    (-QDb[i]/(Vnom[i]^beta_b[i])*((Vrbe[i]^2+Vibe[i]^2)-2*Vrbe[i]^2*(1-beta_b[i]/2))*((Vrbe[i]^2+Vibe[i]^2)^(beta_b[i]/2-2)) + PDb[i]/(Vnom[i]^alfa_b[i])*(-2*(1-alfa_b[i]/2)*Vrbe[i]*Vibe[i])*((Vrbe[i]^2+Vibe[i]^2)^(alfa_b[i]/2-2)))*(Vrb[i,t]-Vrbe[i]) +
		    ( PDb[i]/(Vnom[i]^alfa_b[i])*((Vrbe[i]^2+Vibe[i]^2)-2*Vibe[i]^2*(1-alfa_b[i]/2))*((Vrbe[i]^2+Vibe[i]^2)^(alfa_b[i]/2-2)) - QDb[i]/(Vnom[i]^beta_b[i])*(-2*(1-beta_b[i]/2)*Vrbe[i]*Vibe[i])*((Vrbe[i]^2+Vibe[i]^2)^(beta_b[i]/2-2)))*(Vib[i,t]-Vibe[i]));

	# Calculo da corrente de carga na fase c
		subject to corrente_carga_real_c {i in Ob, t in Ot}:
			IDrc[i,t] = fdem_artigo[t]*((PDc[i]/(Vnom[i]^alfa_c[i])*Vrce[i])*((Vrce[i]^2 + Vice[i]^2)^(alfa_c[i]/2-1)) + (QDc[i]/(Vnom[i]^beta_c[i])*Vice[i])*((Vrce[i]^2 + Vice[i]^2)^(beta_c[i]/2-1))+
		    (PDc[i]/(Vnom[i]^alfa_c[i])*((Vrce[i]^2+Vice[i]^2)-2*Vrce[i]^2*(1-alfa_c[i]/2))*((Vrce[i]^2+Vice[i]^2)^(alfa_c[i]/2-2)) + QDc[i]/(Vnom[i]^beta_c[i])*(-2*(1-beta_c[i]/2)*Vrce[i]*Vice[i])*((Vrce[i]^2+Vice[i]^2)^(beta_c[i]/2-2)))*(Vrc[i,t]-Vrce[i]) +
		    (QDc[i]/(Vnom[i]^beta_c[i])*((Vrce[i]^2+Vice[i]^2)-2*Vice[i]^2*(1-beta_c[i]/2))*((Vrce[i]^2+Vice[i]^2)^(beta_c[i]/2-2)) + PDc[i]/(Vnom[i]^alfa_c[i])*(-2*(1-alfa_c[i]/2)*Vrce[i]*Vice[i])*((Vrce[i]^2+Vice[i]^2)^(alfa_c[i]/2-2)))*(Vic[i,t]-Vice[i]));

		subject to corrente_carga_imag_c {i in Ob, t in Ot}:
		 	IDic[i,t] = fdem_artigo[t]*((PDc[i]/(Vnom[i]^alfa_c[i])*Vice[i])*((Vrce[i]^2 + Vice[i]^2)^(alfa_c[i]/2-1)) - (QDc[i]/(Vnom[i]^beta_c[i])*Vrce[i])*((Vrce[i]^2 + Vice[i]^2)^(beta_c[i]/2-1))+
		    (-QDc[i]/(Vnom[i]^beta_c[i])*((Vrce[i]^2+Vice[i]^2)-2*Vrce[i]^2*(1-beta_c[i]/2))*((Vrce[i]^2+Vice[i]^2)^(beta_c[i]/2-2)) + PDc[i]/(Vnom[i]^alfa_c[i])*(-2*(1-alfa_c[i]/2)*Vrce[i]*Vice[i])*((Vrce[i]^2+Vice[i]^2)^(alfa_c[i]/2-2)))*(Vrc[i,t]-Vrce[i]) +
		    ( PDc[i]/(Vnom[i]^alfa_c[i])*((Vrce[i]^2+Vice[i]^2)-2*Vice[i]^2*(1-alfa_c[i]/2))*((Vrce[i]^2+Vice[i]^2)^(alfa_c[i]/2-2)) - QDc[i]/(Vnom[i]^beta_c[i])*(-2*(1-beta_c[i]/2)*Vrce[i]*Vice[i])*((Vrce[i]^2+Vice[i]^2)^(beta_c[i]/2-2)))*(Vic[i,t]-Vice[i]));

# APARELHOS DE AR CONDICIONADO

	## Inicialização da Temperatura do Ambiente Interno ##

		param Tinicial = 2.0;

		subject to Tin_1_a {n in AC, t in Ot : t = 1 and AC_Fase_a[n] == 1}:
			Tin[n,t,1] = Tset_casa[n] + Tinicial;

		subject to Tin_1_b {n in AC, t in Ot : t = 1 and AC_Fase_b[n] == 1}:
			Tin[n,t,2] = Tset_casa[n] + Tinicial;

		subject to Tin_1_c {n in AC, t in Ot : t = 1 and AC_Fase_c[n] == 1}:
			Tin[n,t,3] = Tset_casa[n] + Tinicial;

		subject to Tin_1_a0 {n in AC, t in Ot : AC_Fase_a[n] == 0}:
			Tin[n,t,1] = 0;

		subject to Tin_1_b0 {n in AC, t in Ot : AC_Fase_b[n] == 0}:
			Tin[n,t,2] = 0;

		subject to Tin_1_c0 {n in AC, t in Ot : AC_Fase_c[n] == 0}:
			Tin[n,t,3] = 0;

	## Inicialização da Temperatura da Parede ##

		subject to Tparede_1_a {n in AC, t in Ot : t = 1 and AC_Fase_a[n] == 1}:
			Tparede[n,t,1] = Tset_casa[n] + Tinicial;

		subject to Tparede_1_b {n in AC, t in Ot : t = 1 and AC_Fase_b[n] == 1}:
			Tparede[n,t,2] = Tset_casa[n] + Tinicial;

		subject to Tparede_1_c {n in AC, t in Ot : t = 1 and AC_Fase_c[n] == 1}:
			Tparede[n,t,3] = Tset_casa[n] + Tinicial;

		subject to Tparede_1_a0 {n in AC, t in Ot : AC_Fase_a[n] == 0}:
			Tparede[n,t,1] = 0;

		subject to Tparede_1_b0 {n in AC, t in Ot : AC_Fase_b[n] == 0}:
			Tparede[n,t,2] = 0;

		subject to Tparede_1_c0 {n in AC, t in Ot : AC_Fase_c[n] == 0}:
			Tparede[n,t,3] = 0;
	
	## Restrições de Conforto ##
	
		subject to Tin_2a {n in AC, t in Ot : t > 1 and AC_Fase_a[n] == 1}:
			Tin[n,t,1] <= Tset_casa[n] + deltaT;

		subject to Tin_2b {n in AC, t in Ot : t > 1 and AC_Fase_b[n] == 1}:
			Tin[n,t,2] <= Tset_casa[n] + deltaT;

		subject to Tin_2c {n in AC, t in Ot : t > 1 and AC_Fase_c[n] == 1}:
			Tin[n,t,3] <= Tset_casa[n] + deltaT;

		subject to Tin_3a {n in AC, t in Ot : t > 1 and AC_Fase_a[n] == 1}:
			Tin[n,t,1] >= Tset_casa[n] - deltaT;

		subject to Tin_3b {n in AC, t in Ot : t > 1 and AC_Fase_b[n] == 1}:
			Tin[n,t,2] >= Tset_casa[n] - deltaT;  

		subject to Tin_3c {n in AC, t in Ot : t > 1 and AC_Fase_c[n] == 1}:
			Tin[n,t,3] >= Tset_casa[n] - deltaT;
	
	## Restrições de Frequência Mínima ##

		subject to frequency_ac_min_a{n in AC, t in Ot : AC_Fase_a[n] == 1}:
			frequency_ac[n,t,1] >= 20 * on_off[n,t,1];
		#	frequency_ac[n,t,1] >= 20;

		subject to frequency_ac_min_b{n in AC, t in Ot : AC_Fase_b[n] == 1}:
			frequency_ac[n,t,2] >= 20 * on_off[n,t,2];
		#	frequency_ac[n,t,2] >= 20;

		subject to frequency_ac_min_c{n in AC, t in Ot : AC_Fase_c[n] == 1}:
			frequency_ac[n,t,3] >= 20 * on_off[n,t,3];
		#	frequency_ac[n,t,3] >= 20;

	## Restrições de Frequência Máxima ##

		subject to frequency_ac_max{n in AC, t in Ot, f in Of}:
			frequency_ac[n,t,f] <= 90 * on_off[n,t,f];
		#	frequency_ac[n,t,f] <= 90;

	## Modificadores ##
	
		subject to modificador_Pac_freq{n in AC, t in Ot, f in Of}:
			mod_Pac_freq[n,t,f] = (0.0136 * frequency_ac[n,t,f] - 0.0456 * on_off[n,t,f]);	 

		subject to modificador_Pac_Tout{t in Ot}: 
			mod_Pac_Tout[t] = (0.0384 * Tout[t] - 0.3436);

		subject to modificador_Hac_freq{n in AC, t in Ot, f in Of}:
			mod_Hac_freq[n,t,f] = (0.0121 * frequency_ac[n,t,f] + 0.0199 * on_off[n,t,f]);

		subject to modificador_Hac_Tout{t in Ot}: 
			mod_Hac_Tout[t] = (-0.0080 * Tout[t] + 1.280);
	
	## Potência Ativa dos Aparelhos de AC ##
	
		subject to restricao_Pac_a{n in AC, t in Ot : AC_Fase_a[n] == 1}:
			Pac[n,t,1] = 0.86 * Pnom_ac[n] * mod_Pac_freq[n,t,1] * mod_Pac_Tout[t];

		subject to restricao_Pac_b{n in AC, t in Ot : AC_Fase_b[n] == 1}:
			Pac[n,t,2] = 0.86 * Pnom_ac[n] * mod_Pac_freq[n,t,2] * mod_Pac_Tout[t];

		subject to restricao_Pac_c{n in AC, t in Ot : AC_Fase_c[n] == 1}:
			Pac[n,t,3] = 0.86 * Pnom_ac[n] * mod_Pac_freq[n,t,3] * mod_Pac_Tout[t];

		subject to restricao_Pac_a0 {n in AC, t in Ot : AC_Fase_a[n] == 0}:
			Pac[n,t,1] = 0;

		subject to restricao_Pac_b0 {n in AC, t in Ot : AC_Fase_b[n] == 0}:
			Pac[n,t,2] = 0;

		subject to restricao_Pac_c0 {n in AC, t in Ot : AC_Fase_c[n] == 0}:
			Pac[n,t,3] = 0;

	## Potência Reativa dos Aparelhos de AC ##

	subject to Potencia_reativa_AC{n in AC, t in Ot, f in Of}:
		Qac[n,t,f] = Pac[n,t,f] * tan(acos(0.91));

	## Potência Térmica dos Aparelhos de AC ##

		subject to restricao_Hac_a{n in AC, t in Ot : AC_Fase_a[n] == 1}:
			Hac[n,t,1] =  1.20 * Pnom_ac[n] * COP_nom[n] * mod_Hac_freq[n,t,1] * mod_Hac_Tout[t];

		subject to restricao_Hac_b{n in AC, t in Ot : AC_Fase_b[n] == 1}:
			Hac[n,t,2] =  1.20 * Pnom_ac[n] * COP_nom[n] * mod_Hac_freq[n,t,2] * mod_Hac_Tout[t];

		subject to restricao_Hac_c{n in AC, t in Ot : AC_Fase_c[n] == 1}:
			Hac[n,t,3] =  1.20 * Pnom_ac[n] * COP_nom[n] * mod_Hac_freq[n,t,3] * mod_Hac_Tout[t];

	## Equações do Modelo Térmico ##

		subject to restricao_a{n in AC}:
			var_a[n] = neper ^ ( - (Ra[n]+Rm[n])/(Ra[n]*Rm[n]*Ca[n]) * dT );

		subject to restricao_b{n in AC}:
			var_b[n] = neper ^ ( - 1 / (Rm[n] * Cm[n]) * dT );

		subject to restricao_Tin_a{n in AC, t in Ot : t > 1 and AC_Fase_a[n] == 1}:
			Tin[n,t,1] = var_a[n] * Tin[n,t-1,1] + (1 - var_a[n]) * Ra[n]/(Ra[n] + Rm[n]) * Tparede[n,t-1,1] +
			(1 - var_a[n])*(Rm[n]/(Ra[n] + Rm[n])*Tout[t-1] - (Ra[n]*Rm[n])/(Ra[n]+Rm[n]) * Hac[n,t-1,1]);

		subject to restricao_Tin_b{n in AC, t in Ot : t > 1 and AC_Fase_b[n] == 1}:
			Tin[n,t,2] = var_a[n] * Tin[n,t-1,2] + (1 - var_a[n]) * Ra[n]/(Ra[n] + Rm[n]) * Tparede[n,t-1,2] +
			(1 - var_a[n])*(Rm[n]/(Ra[n] + Rm[n])*Tout[t-1] - (Ra[n]*Rm[n])/(Ra[n]+Rm[n]) * Hac[n,t-1,2]);

		subject to restricao_Tin_c{n in AC, t in Ot : t > 1 and AC_Fase_c[n] == 1}:
			Tin[n,t,3] = var_a[n] * Tin[n,t-1,3] + (1 - var_a[n]) * Ra[n]/(Ra[n] + Rm[n]) * Tparede[n,t-1,3] +
			(1 - var_a[n])*(Rm[n]/(Ra[n] + Rm[n])*Tout[t-1] - (Ra[n]*Rm[n])/(Ra[n]+Rm[n]) * Hac[n,t-1,3]);

		subject to restricao_Tparede{n in AC, t in Ot, f in Of : t > 1}:
			Tparede[n,t,f] = var_b[n] * Tparede[n,t-1,f] + (1 - var_b[n]) * Tin[n,t-1,f];

	## Equações do Modelo Elétrico ##

		subject to Ire_AC_aprox_linear_a {n in AC, t in Ot}:
			Pac[n,t,1] = Vrae[n] * Iac_re_a[n,t] + Viae[n] * Iac_im_a[n,t];

		subject to Ire_AC_aprox_linear_b {n in AC, t in Ot}:
			Pac[n,t,2] = Vrbe[n] * Iac_re_b[n,t] + Vibe[n] * Iac_im_b[n,t];

		subject to Ire_AC_aprox_linear_c {n in AC, t in Ot}:

			Pac[n,t,3] = Vrce[n] * Iac_re_c[n,t] + Vice[n] * Iac_im_c[n,t];

		subject to Iim_AC_aprox_linear_a {n in AC, t in Ot}:
			Qac[n,t,1] = -Vrae[n] * Iac_im_a[n,t] + Viae[n] * Iac_re_a[n,t];

		subject to Iim_AC_aprox_linear_b {n in AC, t in Ot}:
			Qac[n,t,2] = -Vrbe[n] * Iac_im_b[n,t] + Vibe[n] * Iac_re_b[n,t];

		subject to Iim_AC_aprox_linear_c {n in AC, t in Ot}:
			Qac[n,t,3] = -Vrce[n] * Iac_im_c[n,t] + Vice[n] * Iac_re_c[n,t];

	# Restrições de desconforto

		subject to conforto_1a{n in AC, t in Ot : AC_Fase_a[n] == 1}:
			desconforto[n,t,1] = Taux1[n,t,1] + Taux2[n,t,1];

		subject to conforto_1b{n in AC, t in Ot : AC_Fase_b[n] == 1}:
			desconforto[n,t,2] = Taux1[n,t,2] + Taux2[n,t,2];

		subject to conforto_1c{n in AC, t in Ot : AC_Fase_c[n] == 1}:
			desconforto[n,t,3] = Taux1[n,t,3] + Taux2[n,t,3];

		subject to conforto_2a{n in AC, t in Ot : AC_Fase_a[n] == 0}:
			desconforto[n,t,1] = 0;

		subject to conforto_2b{n in AC, t in Ot : AC_Fase_b[n] == 0}:
			desconforto[n,t,2] = 0;

		subject to conforto_2c{n in AC, t in Ot : AC_Fase_c[n] == 0}:
			desconforto[n,t,3] = 0;

		subject to conforto_3{n in AC, t in Ot, f in Of}:
			Tin[n,t,f] - Tset_casa[n] = Taux1[n,t,f] - Taux2[n,t,f]; 

# END AC

# Modelo das baterias

	# Restricoes de capacidade da bateria

		subject to restricao_bat_0_0{n in BAT, t in Ot, f in Of : t == 1}:
			carga_bateria[n,t,f] = 0;

		subject to restricao_bat_0_1{n in BAT, t in Ot, f in Of}:
			carga_bateria[n,t,f] <= capacidade_bat[n];

		subject to restricao_bat_0_2{n in BAT, t in Ot, f in Of}:
			carga_bateria[n,t,f] >= 0;

		# Restricoes de capacidade da bateria

		subject to restricao_bat_1_0{n in BAT, t in Ot, f in Of : t == card{Ot}}:
			carga_bateria[n,t,f] = 0;

		subject to restricao_bat_1_1{n in BAT, t in Ot, f in Of : t == card{Ot}}:
			carga_bateria[n,t,f] <= capacidade_bat[n];

		subject to restricao_bat_1_2{n in BAT, t in Ot, f in Of : t == card{Ot}}:
			carga_bateria[n,t,f] >= 0;

	# Restricoes de potência da bateria

	subject to restricao_bat_2_1_a{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_a[b] == 1}:
	Pbateria_carga[b,t,1] >= potencia_nom_bat_min[b] * Taux_bat1[b,t,1];
	
	subject to restricao_bat_2_2_a{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_a[b] == 1}:
	Pbateria_carga[b,t,1] <= potencia_nom_bat_max[b] * Taux_bat1[b,t,1];

	subject to restricao_bat_2_1_b{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_b[b] == 1}:
	Pbateria_carga[b,t,2] >= potencia_nom_bat_min[b] * Taux_bat1[b,t,2];
	
	subject to restricao_bat_2_2_b{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_b[b] == 1}:
	Pbateria_carga[b,t,2] <= potencia_nom_bat_max[b] * Taux_bat1[b,t,2];

	subject to restricao_bat_2_1_c{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_c[b] == 1}:
	Pbateria_carga[b,t,3] >= potencia_nom_bat_min[b] * Taux_bat1[b,t,3];
	
	subject to restricao_bat_2_2_c{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_c[b] == 1}:
	Pbateria_carga[b,t,3] <= potencia_nom_bat_max[b] * Taux_bat1[b,t,3];


	subject to restricao_bat_3_1_a{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_a[b] == 1}:
	Pbateria_descarga[b,t,1] >= - potencia_nom_bat_max[b] * (1 - Taux_bat1[b,t,1]);
	
	subject to restricao_bat_3_2_a{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_a[b] == 1}:
	Pbateria_descarga[b,t,1] <=  - potencia_nom_bat_min[b] * (1 - Taux_bat1[b,t,1]);

	subject to restricao_bat_3_1_b{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_b[b] == 1}:
	Pbateria_descarga[b,t,2] >= - potencia_nom_bat_max[b] * (1 - Taux_bat1[b,t,2]);
	
	subject to restricao_bat_3_2_b{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_b[b] == 1}:
	Pbateria_descarga[b,t,2] <=  - potencia_nom_bat_min[b] * (1 - Taux_bat1[b,t,2]);

	subject to restricao_bat_3_1_c{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_c[b] == 1}:
	Pbateria_descarga[b,t,3] >= - potencia_nom_bat_max[b] * (1 - Taux_bat1[b,t,3]);
	
	subject to restricao_bat_3_2_c{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_c[b] == 1}:
	Pbateria_descarga[b,t,3] <=  - potencia_nom_bat_min[b] * (1 - Taux_bat1[b,t,3]);


	subject to restricao_bat_4_1_a{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_a[b] == 1}:
	Qbateria_descarga[b,t,1] >= - potencia_nom_bat_max[b] * (1 - Taux_bat1[b,t,1]);
	
	subject to restricao_bat_4_2_a{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_a[b] == 1}:
	Qbateria_descarga[b,t,1] <= - potencia_nom_bat_min[b] * (1 - Taux_bat1[b,t,1]);

	subject to restricao_bat_4_1_b{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_b[b] == 1}:
	Qbateria_descarga[b,t,2] >= - potencia_nom_bat_max[b] * (1 - Taux_bat1[b,t,2]);
	
	subject to restricao_bat_4_2_b{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_b[b] == 1}:
	Qbateria_descarga[b,t,2] <= - potencia_nom_bat_min[b] * (1 - Taux_bat1[b,t,2]);

	subject to restricao_bat_4_1_c{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_c[b] == 1}:
	Qbateria_descarga[b,t,3] >= - potencia_nom_bat_max[b] * (1 - Taux_bat1[b,t,3]);
	
	subject to restricao_bat_4_2_c{b in BAT, t in Ot, f in Of : t < card(Ot) and BAT_Fase_c[b] == 1}:
	Qbateria_descarga[b,t,3] <= - potencia_nom_bat_min[b] * (1 - Taux_bat1[b,t,3]);

		subject to restricao_bat_6_1{n in BAT, t in Ot, f in Of : t = card{Ot}}:
			Pbateria_carga[n,t,f] = 0;

		subject to restricao_bat_6_2{n in BAT, t in Ot, f in Of : t = card{Ot}}:
			Pbateria_descarga[n,t,f] = 0;

		subject to restricao_bat_6_3{n in BAT, t in Ot, f in Of : t = card{Ot}}:
			Qbateria_descarga[n,t,f] = 0;

		subject to restricao_bat_potencia_aparente{n in BAT, t in Ot, f in Of}:
			(Pbateria_descarga[n,t,f])^2 + (Qbateria_descarga[n,t,f])^2  <= (Sbateria)^2;

	# Equação da bateria

		subject to restricao_bat_5{n in BAT, t in Ot, f in Of : t > 1}:
		carga_bateria[n,t,f] = carga_bateria[n,t-1,f] + eficiencia_carregamento * Pbateria_carga[n,t-1,f] * dT
		+ 1/eficiencia_descarregamento * (Pbateria_descarga[n,t-1,f] + Qbateria_descarga[n,t-1,f]) * dT;

	# Equação da bateria
	
		subject to Ire_BAT_aprox_linear_a{n in BAT, t in Ot}:
			Pbateria_carga[n,t,1] + Pbateria_descarga[n,t,1] = Vrae[n] * Ibat_re_a[n,t] + Viae[n] * Ibat_im_a[n,t];

		subject to Ire_BAT_aprox_linear_b {n in BAT, t in Ot}:
			Pbateria_carga[n,t,2] + Pbateria_descarga[n,t,2] = Vrbe[n] * Ibat_re_b[n,t] + Vibe[n] * Ibat_im_b[n,t];

		subject to Ire_BAT_aprox_linear_c {n in BAT, t in Ot}:
			Pbateria_carga[n,t,3] + Pbateria_descarga[n,t,3] = Vrce[n] * Ibat_re_c[n,t] + Vice[n] * Ibat_im_c[n,t];

		subject to Iim_BAT_aprox_linear_a {n in BAT, t in Ot}:
			Qbateria_descarga[n,t,1] = -Vrae[n] * Ibat_im_a[n,t] + Viae[n] * Ibat_re_a[n,t];

		subject to Iim_BAT_aprox_linear_b {n in BAT, t in Ot}:
			Qbateria_descarga[n,t,2] = -Vrbe[n] * Ibat_im_b[n,t] + Vibe[n] * Ibat_re_b[n,t];

		subject to Iim_BAT_aprox_linear_c {n in BAT, t in Ot}:
			Qbateria_descarga[n,t,3] = -Vrce[n] * Ibat_im_c[n,t] + Vice[n] * Ibat_re_c[n,t];

		subject to Ire_BAT_aprox_linear_a0_ativa_carga {n in BAT, t in Ot : BAT_Fase_a[n] == 0}:
			Pbateria_carga[n,t,1] = 0;

		subject to Ire_BAT_aprox_linear_b0_ativa_carga {n in BAT, t in Ot : BAT_Fase_b[n] == 0}:
			Pbateria_carga[n,t,2] = 0;

		subject to Ire_BAT_aprox_linear_c0_ativa_carga {n in BAT, t in Ot : BAT_Fase_c[n] == 0}:
			Pbateria_carga[n,t,3] = 0;

		subject to Ire_BAT_aprox_linear_a0_ativa_descarga {n in BAT, t in Ot : BAT_Fase_a[n] == 0}:
			Pbateria_descarga[n,t,1] = 0;

		subject to Ire_BAT_aprox_linear_b0_ativa_descarga {n in BAT, t in Ot : BAT_Fase_b[n] == 0}:
			Pbateria_descarga[n,t,2] = 0;

		subject to Ire_BAT_aprox_linear_c0_ativa_descarga {n in BAT, t in Ot : BAT_Fase_c[n] == 0}:
			Pbateria_descarga[n,t,3] = 0;

		subject to Ire_BAT_aprox_linear_a0_reativa {n in BAT, t in Ot : BAT_Fase_a[n] == 0}:
			Qbateria_descarga[n,t,1] = 0;

		subject to Ire_BAT_aprox_linear_b0_reativa {n in BAT, t in Ot : BAT_Fase_b[n] == 0}:
			Qbateria_descarga[n,t,2] = 0;

		subject to Ire_BAT_aprox_linear_c0_reativa {n in BAT, t in Ot : BAT_Fase_c[n] == 0}:
			Qbateria_descarga[n,t,3] = 0;

	# Restricoes de carga e descarga da bateria
		
		subject to limitacao_carga_descarga_bateria_1 {n in BAT, t in Ot, f in Of : t > 1}:
			(carga_bateria[n,t,f] -  carga_bateria[n,t-1,f]) <= Taux_bat2[n,t,f];

		subject to limitacao_carga_descarga_bateria_2 {n in BAT, t in Ot, f in Of : t > 1}:
			-(carga_bateria[n,t,f] -  carga_bateria[n,t-1,f]) <= Taux_bat2[n,t,f];

		subject to limitacao_carga_descarga_bateria_3 {n in BAT, f in Of}:
			sum{t in Ot : t > 1} Taux_bat2[n,t,f] <= 4 * capacidade_bat[n];

# Modelo dos Sistemas Fotovoltaicos

	# Equação do modelo do painel fotovoltaico

		subject to equacao_pfv_a{n in PFV, t in Ot : PFV_Fase_a[n] == 1}:
			pot_pfv[n,t,1] = - num_placas_pfv[n] * area_pfv[n] * eficiencia_pfv[n] *
			(1 - 0.0037 * (Tout[t] - 25)) * Irradiacao_sem_nuvem[t];

		subject to equacao_pfv_b{n in PFV, t in Ot : PFV_Fase_b[n] == 1}:
			pot_pfv[n,t,2] = - num_placas_pfv[n] * area_pfv[n] * eficiencia_pfv[n] *
			(1 - 0.0037 * (Tout[t] - 25)) * Irradiacao_sem_nuvem[t];

		subject to equacao_pfv_c{n in PFV, t in Ot : PFV_Fase_c[n] == 1}:
			pot_pfv[n,t,3] = - num_placas_pfv[n] * area_pfv[n] * eficiencia_pfv[n] *
			(1 - 0.0037 * (Tout[t] - 25)) * Irradiacao_sem_nuvem[t];

	# Equações das casas sem sistemas fotovoltaicos

		subject to Ire_PFV_aprox_linear_a0 {n in PFV, t in Ot : PFV_Fase_a[n] == 0}:
			pot_pfv[n,t,1] = 0;

		subject to Ire_PFV_aprox_linear_b0 {n in PFV, t in Ot : PFV_Fase_b[n] == 0}:
			pot_pfv[n,t,2] = 0;

		subject to Ire_PFV_aprox_linear_c0 {n in PFV, t in Ot : PFV_Fase_c[n] == 0}:
			pot_pfv[n,t,3] = 0;	

	# Equações de Potência Ativa

		subject to Ire_PFV_aprox_linear_a {n in PFV, t in Ot}:
			pot_pfv[n,t,1] = Vrae[n] * Ipfv_re_a[n,t] + Viae[n] * Ipfv_im_a[n,t];

		subject to Ire_PFV_aprox_linear_b {n in PFV, t in Ot}:
			pot_pfv[n,t,2] = Vrbe[n] * Ipfv_re_b[n,t] + Vibe[n] * Ipfv_im_b[n,t];

		subject to Ire_PFV_aprox_linear_c {n in PFV, t in Ot}:
			pot_pfv[n,t,3] = Vrce[n] * Ipfv_re_c[n,t] + Vice[n] * Ipfv_im_c[n,t];

	# Equações de Potência Reativa

		subject to Iim_PFV_aprox_linear_a {n in PFV, t in Ot}:
			0 = -Vrae[n] * Ipfv_im_a[n,t] + Viae[n] * Ipfv_re_a[n,t];

		subject to Iim_PFV_aprox_linear_b {n in PFV, t in Ot}:
			0 = -Vrbe[n] * Ipfv_im_b[n,t] + Vibe[n] * Ipfv_re_b[n,t];

		subject to Iim_PFV_aprox_linear_c {n in PFV, t in Ot}:
			0 = -Vrce[n] * Ipfv_im_c[n,t] + Vice[n] * Ipfv_re_c[n,t];

# Limite de corrente pelos ramos (Linearização)

	# Fase A
	
		subject to corrente_a1 {(i,j) in Ol, t in Ot}:
			Isqra[i,j,t] = sum{l in 1..lambda} ((m[i,j,l] * delra[i,j,t,l]) + (m[i,j,l] * delia[i,j,t,l]));

		subject to corrente_a2 {(i,j) in Ol, t in Ot}:
		 	Ira[i,j,t] = Irap[i,j,t] - Iram[i,j,t];

		subject to corrente_a3 {(i,j) in Ol, t in Ot}: 
		 	Iia[i,j,t] = Iiap[i,j,t] - Iiam[i,j,t];

		subject to corrente_a4 {(i,j) in Ol, t in Ot}:
		 	Irap[i,j,t] + Iram[i,j,t] = sum{l in 1..lambda} delra[i,j,t,l];

		subject to corrente_a5 {(i,j) in Ol, t in Ot}:
		 	Iiap[i,j,t] + Iiam[i,j,t] = sum{l in 1..lambda} delia[i,j,t,l];

		subject to corrente_a6 {(i,j) in Ol, t in Ot, l in 1..lambda}:
		 	delra[i,j,t,l] <= delmax[i,j];

		subject to corrente_a7 {(i,j) in Ol, t in Ot, l in 1..lambda}:
		 	delia[i,j,t,l] <= delmax[i,j];

		subject to corrente_a8 {(i,j) in Ol, t in Ot}:
		 	0 <= Isqra[i,j,t] <= Imax[i,j]^2;
	
	# Fase B

		subject to corrente_b1 {(i,j) in Ol, t in Ot}:
			 Isqrb[i,j,t] = sum{l in 1..lambda} (m[i,j,l] * delrb[i,j,t,l]) + sum{l in 1..lambda} (m[i,j,l] * delib[i,j,t,l]);

		subject to corrente_b2 {(i,j) in Ol, t in Ot}:
		 	Irb[i,j,t] = Irbp[i,j,t] - Irbm[i,j,t];

		subject to corrente_b3 {(i,j) in Ol, t in Ot}: 
			 Iib[i,j,t] = Iibp[i,j,t] - Iibm[i,j,t];

		subject to corrente_b4 {(i,j) in Ol, t in Ot}:
		 	Irbp[i,j,t] + Irbm[i,j,t] = sum{l in 1..lambda} delrb[i,j,t,l];

		subject to corrente_b5 {(i,j) in Ol, t in Ot}:
		 	Iibp[i,j,t] + Iibm[i,j,t] = sum{l in 1..lambda} delib[i,j,t,l];

		subject to corrente_b6 {(i,j) in Ol, l in 1..lambda, t in Ot}:
		 	delrb[i,j,t,l] <= delmax[i,j];

		subject to corrente_b7 {(i,j) in Ol, l in 1..lambda, t in Ot}:
		 	delib[i,j,t,l] <= delmax[i,j];

		subject to corrente_b8 {(i,j) in Ol, t in Ot}:
			0 <= Isqrb[i,j,t] <= Imax[i,j]^2;
	
	# Fase C

		subject to corrente_c1 {(i,j) in Ol, t in Ot}:
			Isqrc[i,j,t] = sum{l in 1..lambda} (m[i,j,l] * delrc[i,j,t,l]) + 
			sum{l in 1..lambda} (m[i,j,l] * delic[i,j,t,l]);

		subject to corrente_c2 {(i,j) in Ol, t in Ot}:
			Irc[i,j,t] = Ircp[i,j,t] - Ircm[i,j,t];

		subject to corrente_c3 {(i,j) in Ol, t in Ot}: 
			Iic[i,j,t] = Iicp[i,j,t] - Iicm[i,j,t];

		subject to corrente_c4 {(i,j) in Ol, t in Ot}:
			Ircp[i,j,t] + Ircm[i,j,t] = sum{l in 1..lambda} delrc[i,j,t,l];

		subject to corrente_c5 {(i,j) in Ol, t in Ot}:
			Iicp[i,j,t] + Iicm[i,j,t] = sum{l in 1..lambda} delic[i,j,t,l];

		subject to corrente_c6 {(i,j) in Ol, l in 1..lambda, t in Ot}:
			delrc[i,j,t,l] <= delmax[i,j];

		subject to corrente_c7 {(i,j) in Ol, l in 1..lambda, t in Ot}:
			delic[i,j,t,l] <= delmax[i,j];

		subject to corrente_c8 {(i,j) in Ol, t in Ot}:
			0 <= Isqrc[i,j,t] <= Imax[i,j]^2;

# Limite de Tensão dos nós (Linearização)	

	# Fase A

		subject to limite_tensao_a1 {i in Ob, t in Ot}:
		 	Via[i,t] <= ((sin(tha+th2)-sin(tha-th1))/(cos(tha+th2)-cos(tha-th1)))*(Vra[i,t]-Vmin[i]*cos(tha+th2))+Vmin[i]*sin(tha+th2);

		subject to limite_tensao_a2 {i in Ob, t in Ot}:
		 	Via[i,t] <= ((sin(tha+th2)-sin(tha))/(cos(tha+th2)-cos(tha)))*(Vra[i,t]-Vmax[i]*cos(tha))+Vmax[i]*sin(tha);

		subject to limite_tensao_a3 {i in Ob, t in Ot}:
		 	Via[i,t] >= ((sin(tha-th1)-sin(tha))/(cos(tha-th1)-cos(tha)))*(Vra[i,t]-Vmax[i]*cos(tha))+Vmax[i]*sin(tha);

		subject to limite_tensao_a4 {i in Ob, t in Ot}:
		 	Via[i,t] <= Vra[i,t]*tan(tha+th2);

		subject to limite_tensao_a5 {i in Ob, t in Ot}:
		 	Via[i,t] >= Vra[i,t]*tan(tha-th1);
	
	# Fase B

		subject to limite_tensao_b1 {i in Ob, t in Ot}:
	 	Vib[i,t] <= ((sin(thb+th2)-sin(thb-th1))/(cos(thb+th2)-cos(thb-th1)))*(Vrb[i,t]-Vmin[i]*cos(thb+th2))+Vmin[i]*sin(thb+th2);
	
		subject to limite_tensao_b2 {i in Ob, t in Ot}:
		 Vib[i,t] >= ((sin(thb+th2)-sin(thb))/(cos(thb+th2)-cos(thb)))*(Vrb[i,t]-Vmax[i]*cos(thb))+Vmax[i]*sin(thb);
	
		subject to limite_tensao_b3 {i in Ob, t in Ot}:
		 	Vib[i,t] >= ((sin(thb-th1)-sin(thb))/(cos(thb-th1)-cos(thb)))*(Vrb[i,t]-Vmax[i]*cos(thb))+Vmax[i]*sin(thb);

		subject to limite_tensao_b4 {i in Ob, t in Ot}:
		 	Vib[i,t] >= Vrb[i,t]*tan(thb+th2);

		subject to limite_tensao_b5 {i in Ob, t in Ot}:
		 	Vib[i,t] <= Vrb[i,t]*tan(thb-th1);
	
	# Fase C

		subject to limite_tensao_c1 {i in Ob, t in Ot}:
		 	Vic[i,t] >= ((sin(thc+th2)-sin(thc-th1))/(cos(thc+th2)-cos(thc-th1)))*(Vrc[i,t]-Vmin[i]*cos(thc+th2))+Vmin[i]*sin(thc+th2);

		subject to limite_tensao_c2 {i in Ob, t in Ot}:
		 	Vic[i,t] <= ((sin(thc+th2)-sin(thc))/(cos(thc+th2)-cos(thc)))*(Vrc[i,t]-Vmax[i]*cos(thc))+Vmax[i]*sin(thc);

		subject to limite_tensao_c3 {i in Ob, t in Ot}:
		 	Vic[i,t] <= ((sin(thc-th1)-sin(thc))/(cos(thc-th1)-cos(thc)))*(Vrc[i,t]-Vmax[i]*cos(thc))+Vmax[i]*sin(thc);

		subject to limite_tensao_c4 {i in Ob, t in Ot}:
		 	Vic[i,t] >= Vrc[i,t]*tan(thc+th2);

		subject to limite_tensao_c5 {i in Ob, t in Ot}:
		 	Vic[i,t] <= Vrc[i,t]*tan(thc-th1);

## Consumo interno de cada residência

#	subject to consumo_interno_energia_ativa_a{i in AC, t in Ot : AC_Fase_a[i] == 1}:
#			PCa[i,t] = Pac[i,t,1] + Pbateria_carga[i,t,1] + Pbateria_descarga[i,t,1] + pot_pfv[i,t,1];
#
#	subject to consumo_interno_energia_ativa_b{i in AC, t in Ot : AC_Fase_b[i] == 1}:
#			PCb[i,t] = Pac[i,t,2] + Pbateria_carga[i,t,2] + Pbateria_descarga[i,t,2] + pot_pfv[i,t,2];
#
#	subject to consumo_interno_energia_ativa_c{i in AC, t in Ot : AC_Fase_c[i] == 1}:
#			PCc[i,t] = Pac[i,t,3] + Pbateria_carga[i,t,3] + Pbateria_descarga[i,t,3] + pot_pfv[i,t,3];
#
#	subject to consumo_interno_energia_reativa_a{i in AC, t in Ot : AC_Fase_a[i] == 1}:
#			QCa[i,t] = Qac[i,t,1] + Qbateria_descarga[i,t,1];
#
#	subject to consumo_interno_energia_reativa_b{i in AC, t in Ot : AC_Fase_b[i] == 1}:
#			QCb[i,t] = Qac[i,t,2] + Qbateria_descarga[i,t,2];
#
#	subject to consumo_interno_energia_reativa_c{i in AC, t in Ot : AC_Fase_c[i] == 1}:
#			QCc[i,t] = Qac[i,t,3] + Qbateria_descarga[i,t,3];