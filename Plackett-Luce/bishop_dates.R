#dates
qnames$from=integer(NP)
qnames$to=integer(NP)

doi.i=which(qnames$diocese=="Coutances")
qnames$name[doi.i]
#[1] "Geoffrey, bishop of Coutances"              
qnames$from[doi.i[1]]=1049
qnames$to[doi.i[1]]=1093
#[2] "Robert, bishop of Coutances"                
qnames$from[doi.i[2]]=1093
qnames$to[doi.i[2]]=1110
#[3] "Algar, Bishop of Coutances"                 
qnames$from[doi.i[3]]=1132
qnames$to[doi.i[3]]=1151
#[4] "Richard, Bishop of Coutances, temp. Stephen"
qnames$from[doi.i[4]]=1151
qnames$to[doi.i[4]]=1179
#[5] "Richard, bishop of Coutances"               
qnames$from[doi.i[5]]=1124
qnames$to[doi.i[5]]=1131
#[6] "Roger, bishop of Coutances"  
qnames$from[doi.i[6]]=1114
qnames$to[doi.i[6]]=1123

doi.i=which(qnames$diocese=="Chester")
qnames$name[doi.i]
#[1] "Peter, bishop of Chester"                     
qnames$from[doi.i[1]]=1075
qnames$to[doi.i[1]]=1085
#[2] "Robert, de Limesey, bishop of Chester"        
qnames$from[doi.i[2]]=1086
qnames$to[doi.i[2]]=1117
#[3] "Roger, de Clinton, Bishop of Chester"         
qnames$from[doi.i[3]]=1129
qnames$to[doi.i[3]]=1148
#[4] "Walter, Durdent, Bishop of Chester-Coventry"  
qnames$from[doi.i[4]]=1149
qnames$to[doi.i[4]]=1159
#[5] "Robert, Peche, bishop of Chester, 1121-6"     
qnames$from[doi.i[5]]=1121
qnames$to[doi.i[5]]=1126
#[6] "Robert, de Limesy or Peche, bishop of Chester"
qnames$from[doi.i[6]]=NA
qnames$to[doi.i[6]]=NA

doi.i=which(qnames$diocese=="London")
qnames$name[doi.i]
#[1] "Maurice, bishop of London"    
qnames$from[doi.i[1]]=1085
qnames$to[doi.i[1]]=1107
#[2] "William, bishop of London" 
qnames$from[doi.i[2]]=1051
qnames$to[doi.i[2]]=1075
#[3] "Hugh, bishop of London"
qnames$from[doi.i[3]]=1075
qnames$to[doi.i[3]]=1085
#[4] "Richard, de Belmeis II, bishop of London 1152-1162"
qnames$from[doi.i[4]]=1152
qnames$to[doi.i[4]]=1162
#[5] "R.,, unidentified, bishop of London, temp. Stephen"
qnames$from[doi.i[5]]=NA
qnames$to[doi.i[5]]=NA
#[6] "Robert, de Sigillo, Bishop of London"              
qnames$from[doi.i[6]]=1141
qnames$to[doi.i[6]]=1150
#[7] "Richard, de Belmeis I, bishop of London, 1108-1127"
qnames$from[doi.i[7]]=1108
qnames$to[doi.i[7]]=1127
#[8] "Gilbert, the Universal, bishop of London"
qnames$from[doi.i[8]]=1128
qnames$to[doi.i[8]]=1134

doi.i=which(qnames$diocese=="Worcester")
qnames$name[doi.i]
#[1] "Wulfstan, bishop of Worcester"  
qnames$from[doi.i[1]]=1062
qnames$to[doi.i[1]]=1095
#[2] "Samson, bishop of Worcester"
qnames$from[doi.i[2]]=1096
qnames$to[doi.i[2]]=1112
#[3] "Simon, Bishop of Worcester"
qnames$from[doi.i[3]]=1125
qnames$to[doi.i[3]]=1150
#[4] "John, of Pagham, Bishop of Worcester"
qnames$from[doi.i[4]]=1151
qnames$to[doi.i[4]]=1157
#[5] "Theulf, Bishop of Worcester"
qnames$from[doi.i[5]]=1113
qnames$to[doi.i[5]]=1123

doi.i=which(qnames$diocese=="Winchester")
qnames$name[doi.i]
#[1] "Walkelin, bishop of Winchester, 1070-1198"        
qnames$from[doi.i[1]]=1070
qnames$to[doi.i[1]]=1098
#[2] "William, Giffard, bishop of Winchester, 1100-1129"
qnames$from[doi.i[2]]=1100
qnames$to[doi.i[2]]=1129
#[3] "Henry, de Blois, Bishop of Winchester, 1129-1171" #well... it was October 1129
qnames$from[doi.i[3]]=1130
qnames$to[doi.i[3]]=1171

doi.i=which(qnames$diocese=="Wells")
qnames$name[doi.i]
#[1] "Giso, bishop of Wells"
qnames$from[doi.i[1]]=1060
qnames$to[doi.i[1]]=1088

doi.i=which(qnames$diocese=="Tusculum")
qnames$name[doi.i]
#[1] "John, bishop of Tusculum"
qnames$from[doi.i[1]]=1100
qnames$to[doi.i[1]]=1120

doi.i=which(qnames$diocese=="Thetford")
qnames$name[doi.i]
#[1] "Arfastus, bishop of Thetford"   
qnames$from[doi.i[1]]=1070
qnames$to[doi.i[1]]=1085
#[2] "William, de Beaufeu, bishop of Thetford"
qnames$from[doi.i[2]]=1085
qnames$to[doi.i[2]]=1091

doi.i=which(qnames$diocese=="the Orkneys")
qnames$name[doi.i]
#"Ralph, bishop of the Orkneys"
#insecure hold on see at best - dates ill-defined
qnames$from[doi.i[1]]=1109
qnames$to[doi.i[1]]=1134

doi.i=which(qnames$diocese=="St Davids")
qnames$name[doi.i]
#"Bernard, Bishop of St David's"
qnames$from[doi.i[1]]=1115
qnames$to[doi.i[1]]=1148

doi.i=which(qnames$diocese=="St Asaph")
qnames$name[doi.i]
#"Geoffrey, Bishop of St Asaph"
qnames$from[doi.i[1]]=1152
qnames$to[doi.i[1]]=1155

doi.i=which(qnames$diocese=="Sherborne")
qnames$name[doi.i]
#"Herman, bishop of Sherborne"
qnames$from[doi.i[1]]=1058
qnames$to[doi.i[1]]=1078

doi.i=which(qnames$diocese=="Sees")
qnames$name[doi.i]
#"Robert, bishop of Séez" 
qnames$from[doi.i[1]]=1070
qnames$to[doi.i[1]]=1081
#"John, Bishop of Sees"
qnames$from[doi.i[2]]=1124
qnames$to[doi.i[2]]=1143

doi.i=which(qnames$diocese=="Salisbury")
qnames$name[doi.i]
#"Osmund, bishop of Salisbury"           
qnames$from[doi.i[1]]=1078
qnames$to[doi.i[1]]=1099
#[3] "Roger, Bishop of Salisbury"
qnames$from[doi.i[2]]=1102
qnames$to[doi.i[2]]=1139
# "Jocelin, de Bohun, bishop of Salisbury"
qnames$from[doi.i[3]]=1142
qnames$to[doi.i[3]]=1184

doi.i=which(qnames$diocese=="Rochester")
qnames$name[doi.i]
#[1] "Gundulf, bishop of Rochester"
qnames$from[doi.i[1]]=1077
qnames$to[doi.i[1]]=1108
#[2] "John, Bishop of Rochester" 
qnames$from[doi.i[2]]=1125
qnames$to[doi.i[2]]=1137
#[3] "Walter, Bishop  of Rochester"
qnames$from[doi.i[3]]=1148
qnames$to[doi.i[3]]=1182
#[4] "Arnulf, bishop of Rochester" 
qnames$from[doi.i[4]]=1114
qnames$to[doi.i[4]]=1124

doi.i=which(qnames$diocese=="Norwich")
qnames$name[doi.i]
#"Everard, bishop of Norwich"  
qnames$from[doi.i[1]]=1121
qnames$to[doi.i[1]]=1145
#"William, Turbe, Bishop of Norwich"
qnames$from[doi.i[2]]=1146
qnames$to[doi.i[2]]=1174

doi.i=which(qnames$diocese=="Llandaff")
qnames$name[doi.i]
#"Urban, bishop of Llandaff"
qnames$from[doi.i[1]]=1107
qnames$to[doi.i[1]]=1134


doi.i=which(qnames$diocese=="Lisieux")
qnames$name[doi.i]
#[1] "Hugh, bishop of Lisieux"   
qnames$from[doi.i[1]]=1049
qnames$to[doi.i[1]]=1077
#[2] "Gilbert, bishop of Lisieux"
qnames$from[doi.i[2]]=1077
qnames$to[doi.i[2]]=1101
#[3] "Arnulf, Bishop of Lisieux" 
qnames$from[doi.i[3]]=1142
qnames$to[doi.i[3]]=1181
#[4] "John, Bishop of Lisieux"  
qnames$from[doi.i[4]]=1107
qnames$to[doi.i[4]]=1141

doi.i=which(qnames$diocese=="Lincoln")
qnames$name[doi.i]
#"Remigius, bishop of Lincoln"
qnames$from[doi.i[1]]=1072
qnames$to[doi.i[1]]=1092
#"Robert, Bloet, bishop of Lincoln"   
qnames$from[doi.i[2]]=1093
qnames$to[doi.i[2]]=1122
#"Alexander, Bishop of Lincoln"
qnames$from[doi.i[3]]=1123
qnames$to[doi.i[3]]=1148
#"Robert, de Chesney, Bishop of Lincoln"
qnames$from[doi.i[4]]=1148
qnames$to[doi.i[4]]=1166

doi.i=which(qnames$diocese=="Le Mans")
qnames$name[doi.i]
#"Arnold, bishop of Le Mans"
qnames$from[doi.i[1]]=1067
qnames$to[doi.i[1]]=1081
#"Hoellus, bishop of Le Mans"
qnames$from[doi.i[2]]=1085
qnames$to[doi.i[2]]=1097

doi.i=which(qnames$diocese=="Hereford")
qnames$name[doi.i]
#[1] "Robert, Losinga, bishop of Hereford"  
qnames$from[doi.i[1]]=1079
qnames$to[doi.i[1]]=1095
#[2] "Walter, bishop of Hereford"
qnames$from[doi.i[2]]=1061
qnames$to[doi.i[2]]=1079
#[3] "Robert, de Bethune, Bishop of Hereford"
qnames$from[doi.i[3]]=1131
qnames$to[doi.i[3]]=1148
#[4] "Reinhelm, bishop of Hereford" 
qnames$from[doi.i[4]]=1107
qnames$to[doi.i[4]]=1115
#[5] "Richard, de Capella, bishop of Hereford"
qnames$from[doi.i[5]]=1121
qnames$to[doi.i[5]]=1127


doi.i=which(qnames$diocese=="Exeter")
qnames$name[doi.i]
#[1] "Osbern, bishop of Exeter"
qnames$from[doi.i[1]]=1072
qnames$to[doi.i[1]]=1103
#"Leofric, bishop of Exeter" 
qnames$from[doi.i[2]]=1050
qnames$to[doi.i[2]]=1072
#"William, de Warelwast, bishop of Exeter"
qnames$from[doi.i[3]]=1107
qnames$to[doi.i[3]]=1137
#[4] "Robert, Bishop of Exeter" 
qnames$from[doi.i[4]]=1138
qnames$to[doi.i[4]]=1155

doi.i=which(qnames$diocese=="Evreux")
qnames$name[doi.i]
#[1] "Gilbert, bishop of Evreux"
qnames$from[doi.i[1]]=1071
qnames$to[doi.i[1]]=1112
#[2] "Ouen, Bishop of Evreux"
qnames$from[doi.i[2]]=1113
qnames$to[doi.i[2]]=1139
#[3] "Rotrou, Bishop of Evreux" 
qnames$from[doi.i[3]]=1140
qnames$to[doi.i[3]]=1165

doi.i=which(qnames$diocese=="Ely")
qnames$name[doi.i]
#"Nigel, Bishop of Ely"
qnames$from[doi.i[1]]=1133
qnames$to[doi.i[1]]=1169

doi.i=which(qnames$diocese=="Durham")
qnames$name[doi.i]
#[1] "William, de Saint-Calais, bishop of Durham"
qnames$from[doi.i[1]]=1081
qnames$to[doi.i[1]]=1096
#[2] "Walcher, bishop of Durham"
qnames$from[doi.i[2]]=1071
qnames$to[doi.i[2]]=1080
#[3] "Ranulf, Flambard, bishop of Durham"
qnames$from[doi.i[3]]=1099
qnames$to[doi.i[3]]=1128
#[4] "Geoffrey, Rufus, Bishop of Durham" 
qnames$from[doi.i[4]]=1133
qnames$to[doi.i[4]]=1140
#[5] "Hugh, du Puiset, Bishop of Durham"
qnames$from[doi.i[5]]=1153
qnames$to[doi.i[5]]=1195

doi.i=which(qnames$diocese=="Chichester")
qnames$name[doi.i]
#[1] "Stigand, bishop of Chichester"    
qnames$from[doi.i[1]]=1075
qnames$to[doi.i[1]]=1087
#[2] "Ralph, bishop of Chichester"  
qnames$from[doi.i[2]]=1091
qnames$to[doi.i[2]]=1123
#[3] "Seffrid, Bishop of Chichester"     
qnames$from[doi.i[3]]=1125
qnames$to[doi.i[3]]=1145
#[4] "Hilary, Bishop of Chichester 1146-1169"
qnames$from[doi.i[4]]=1147
qnames$to[doi.i[4]]=1169

doi.i=which(qnames$diocese=="Carlisle")
qnames$name[doi.i]
#"Adelulf, Bishop of Carlisle"
qnames$from[doi.i[1]]=1133
qnames$to[doi.i[1]]=1156

doi.i=which(qnames$diocese=="Bayeux")
qnames$name[doi.i]
#[1] "Odo, bishop of Bayeux"
qnames$from[doi.i[1]]=1049
qnames$to[doi.i[1]]=1097
#[2] "Philip, Bishop of Bayeux"
qnames$from[doi.i[2]]=1142
qnames$to[doi.i[2]]=1163
#[3] "Richard, Bishop of Bayeux" 
qnames$from[doi.i[3]]=1107
qnames$to[doi.i[3]]=1133
#[4] "Turold, de Envermeu, bishop of Bayeux"
qnames$from[doi.i[4]]=1097
qnames$to[doi.i[4]]=1106

doi.i=which(qnames$diocese=="Bath")
qnames$name[doi.i]
#[1] "John, bishop of Bath" 
qnames$from[doi.i[1]]=1090
qnames$to[doi.i[1]]=1122
#[2] "Robert, of Lewes, Bishop of Bath"
qnames$from[doi.i[2]]=1136
qnames$to[doi.i[2]]=1166
#[3] "Godfrey, bishop of Bath" 
qnames$from[doi.i[3]]=1123
qnames$to[doi.i[3]]=1135

doi.i=which(qnames$diocese=="Bangor")
qnames$name[doi.i]
#"Hervey, bishop of Bangor"
#actually only bishop of B. till 1108 then Ely.
qnames$from[doi.i[1]]=1092
qnames$to[doi.i[1]]=1131
#"David, bishop of Bangor"
qnames$from[doi.i[2]]=1120
qnames$to[doi.i[2]]=1139

doi.i=which(qnames$diocese=="Avranches")
qnames$name[doi.i]
#[1] "Michael, bishop of Avranches"
qnames$from[doi.i[1]]=1068
qnames$to[doi.i[1]]=1094
#[2] "John, bishop of Avranches" 
qnames$from[doi.i[2]]=1060
qnames$to[doi.i[2]]=1067
#[3] "Richard, de Beaufeu, Bishop of Avranches"
qnames$from[doi.i[3]]=1134
qnames$to[doi.i[3]]=1142
#[4] "Turgis, bishop of Avranches"
qnames$from[doi.i[4]]=1094
qnames$to[doi.i[4]]=1134

write.csv(qnames,file="BishopDates.csv")
