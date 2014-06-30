A>B indica che B discende da A ed è quindi, tendenzialmente, più aggiornato
Xrotor_modVISUAL - noPlot : tolta la parte grafica, il resto è uguale
Xrotor_modVISUAL - noPlot>Xrotor_modVISUAL - InputMod : come predecessore, in più acquisisce il file di Matlab in input e salva il file output.txt
Xrotor_modVISUAL - noPlot>Xrotor_modVISUAL - InputMod> Xrotor_modVISUAL - InputOutputMod: come predecessori, in più è possibile specificare il nome del file di output come secondo parametro, per immettere casefile si passa il terzo parametro
Xrotor_modVISUAL - noPlot>Xrotor_modVISUAL - InputMod> Xrotor_modVISUAL - InputOutputMod>Xrotor_modVISUAL - Complete: come predecessori, ultimato il casefile per convergere con qualsiasi coppia di parametri immessi
Xrotor_modVISUAL - noPlot>Xrotor_modVISUAL - InputMod> Xrotor_modVISUAL - InputOutputMod>Xrotor_modVISUAL - Complete>Xrotor_modVISUAL - Complete2: copia di backup prima di aggiungere l'oper copiato da xrotor originale

Xrotor_modVISUAL - Final : ultima versione disponibile per la 7.55

Xrotor7.3 : versione precedente alla 7.55 senza plot
Xrotor7.3>Xrotor730MOD : versione 7.3 riadattata per funzionare con Matlab alla pari della versione Final con in più la possibilità di autoiterare

Xrotor_modVISUAL : aggiunte le modifiche fatte alla 7.3 con autoiterazione e corretto l'accidentale errore per la convergenza a Torque e Power