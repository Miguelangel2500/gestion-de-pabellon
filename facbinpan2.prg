*set default to \\BELEN\visprogfac2           
SET DEFAULT TO c:\visprogfac2
SET EXCLUSIVE OFF
SET POINT TO ","
SET SEPARATOR TO "."
SET CURSOR ON
WMAYUS=CAPSLOCK()
*Push MENU _MSYSMENU
*Set SYSMENU OFF
CAPSLOCK(.T.)
_SCREEN.ScrollBars= 3

_screen.FontSize=15
_screen.FontName="COURIER NEW"
*_SCREEN.FontName="Monospac821 BT"
*_screen.ControlBox= .F.
*_screen.WindowState= 2

*_screen.TitleBar= 0
_screen.Caption="Programa Facturas "
*_screen.Visible= .F.

*_SCREEN.BackColor= RGB(255,128,255) 

*************************
**************************************
******** indices
*
*
*   facturas
*
*         infac   factura
*         inclife cliente+str(year(fecha),4)+str(month(fecha),2)+str(day(fecha),2)
*
*  clientes
*
*         inclave  clave
*         innombre nombre
*         innif    nif   
*
*  conceptos
*
*         infacfec   numfac+str(year(fecha),4)+str(month(fecha),2)+str(day(fecha),2)
*         innumero   str(numero,7)
*         in1ref     referencia
*         in1cmr     CMR
******en subprograma resumen camion indice temporal conceptos intemp1

***************************************************

SET CENTURY ON
SET TALK On
SET ECHO On
SET STEP On
SET DATE ITALIAN

set deleted on
************************ bucle principal
do while .t.

clear
PUBLIC vmenu as String

vmenu="s"
*@ 2,60  SAY "PROGRAMA FACTURAS   (EPSON STYLUS)"
*@ 3,14  to 28,60
*@ 3,52  TO 3,79
*@ 4,15  say "(F)acturacion  "
*@ 6,15  say "Señalar facturas (p)agadas"
*@ 8,15  say "(L)istado de facturas impagadas"
*@ 10,15 say "(C)onsulta de un cliente"
*@ 12,15 say "Busq(u)eda por numero referencia"
*@ 14,15 say "(M)antenimiento de clientes"
*@ 16,15 say "Listado de (t)odas las facturas"
*@ 18,15 say "(R)esumen mensual de vehiculos"
*@ 20,15 SAY "Salida (A)guado"
*@ 22,15 SAY "Reinde(x)ar"
*@ 24,15 say "(Q)uit"
*@ 26,15 say "(S)alir del programa          " get vmenu
DO FORM VISPROGFAC_menuinicio

*@ 1,1 to 30,95 double
*read
vmenu=lower(vmenu)

do case
***************** principal
CASE VMENU=="cp"
     DO viajprov


CASE vmenu="k"
     DO fnacrep

CASE VMENU="d"

     DO MODIFAC

case vmenu="f"
******************


    CLEAR

    STORE SPACE(5) TO wnumfac
    STORE SPACE(15) TO wempresa
    @ 9,20 say "numero de factura " get wnumfac
    *@ 10,20 say "empresa emisora si no es cabrero " get wempresa
    READ
    *WEMPRESA=UPPER(WEMPRESA)
    
    *DO FORM visprogfac_numerofactura
    
    *WNUMFAC=INPUTBOX("Numero de factura:   ")
    *wnumfac=upper(TRIM(wnumfac))

    respuesta="o"
    USE FACTURAS INDEX INFAC,inclife

    seek WNUMFAC

    ****si la factura ya esta en los ficheros
    IF FOUND()
         fechafac=fecha
         respuesta="c"
         CLEAR
         *@ 1,1 TO 30,90 DOUBLE
         *@ 5,19 TO 12,77
         *@ 6,20 say "Cliente: " +cliente
         *@ 8,20 say "Factura: " +factura+"                     Fecha fac.:"+dtoc(fechafac)
         do case
           case pagado
               wpagado="Pagado"
           case .not. pagado
               wpagado="No pagado"
         endcase
         *@ 10,20 say "Importe: " +str(importe,10)+" "+moneda +" "+wpagado+"  Fecha env.:"+dtoc(envfecha)
         *@ 14,19 TO 25,70
         *@ 15,20 SAY "La factura ya existe; ¿Desea (b)orrarla?"
         *@ 17,20 say "¿(I)mprimirla de nuevo?"
         *@ 19,20 say "(M)odificar"
         *@ 21,20 say "¿Consulta(r) la factura?"
         *@ 23,20 say "¿(C)ontinuar?                           " get respuesta
         *read
         DO FORM visprogfac_menufacturaencontrada
         respuesta = lower(respuesta)
         do case

               case respuesta="r"

                 wclave=cliente
                 fechafac=fecha
                 wnota1=nota1
                 wnota2=nota2
                 wnota3=nota3
                 wnota4=nota4
                 wnota5=nota5
                 wnota6=nota6
                 wnota7=nota7
                 WMODALIDAD=MODALIDAD
                   do faconsul with wnumfac,WMODALIDAD

               case respuesta="m"

                 wclave=cliente
                 fechafac=fecha
                 wnota1=nota1
                 wnota2=nota2
                 wnota3=nota3
                 wnota4=nota4
                 wnota5=nota5
                 wnota6=nota6
                 wnota7=nota7
                 WMODALIDAD=MODALIDAD
                   do modif with wnumfac,WMODALIDAD
               case respuesta="b"
                   clear
                   *contestacion="n"
                   *@ 5,20 say "Factura: " +wnumfac
                   *@ 6,20 say "¿Esta seguro de que desea borrarla (s/n)?" get contestacion
                   *read
                   contestacion=STR(MESSAGEBOX("Factura: "+wnumfac+CHR(13)+"¿Esta seguro de que desea borrarla?",4),1)
                   *contestacion=lower(contestacion)
                   *contestacion=STR(MESSAGEBOX(),1)
                   if contestacion="6"
                     delete for factura=wnumfac
                     
                     use concepto index infacfec,innumero
                     delete all for numfac=wnumfac
                     


                     wait WINDOW "         La factura ha sido borrada"
                     
                   else
                     
                   endif
               case respuesta="i"
                 use facturas index infac,inclife

                 seek wnumfac
                 *****grabamos en variables de trabajo
                 *****los datos de la factura
                 wnformapago=formapago
                 if formapago=" "
                   clear
                   store space(50) to wnformapago
                   wclave=cliente
                   use clientes index inclave,innombre,innif
                   seek wclave
                   wnformapago=formapago

                   @ 9,5 say "Modificar o añadir forma de pago"
                   @ 10,5 say " " get wnformapago
                   read
                   replace formapago with wnformapago
                   use facturas index infac,inclife

                   SEEK wnumfac
                 endif
                 wclave=cliente
                 fechafac=fecha
                 wnota1=nota1
                 wnota2=nota2
                 wnota3=nota3
                 wnota4=nota4
                 wnota5=nota5
                 wnota6=nota6
                 wnota7=nota7
                 wmodalidad=modalidad
                 *******buscamos los datos del cliente

                 use clientes index inclave,innombre,innif

                 seek wclave

                 ****como el cliente ya esta en el fichero, grabamos
                 *en variables de trabajo los datos de la factura y el
                 ***cliente
                 clear
                 
                 wnombre    = nombre
                 wdireccion = direccion
                 wpoblacion = poblacion
                 wprovincia = provincia
                 wcodpostal = codpostal
                 wpais      = pais
                 wnif       = nif
                 WAPDO      = APDO
                 
                 wformapago = formapago
                 wpoligono  =poligono
                 if wmodalidad=" "
                      wmodalidad="n"
                      clear
                      @ 1,1 TO 24,79 DOUBLE
                      *@ 2,1 TO 17,70
                      @ 3,10 SAY "Cliente: " +wnombre
                      @ 5,10 say "n(o)rmal"
			          @ 7,10 say "(a)rcese"
			          @ 9,10 say "(e)xtranjero"
			          *@ 14,10 say "(n)acional 16%"
			          @ 14,10 SAY "8%(r)"
			          @ 12,10 SAY "REDUCIDO 10%(k)"
			          @ 13,10 SAY "NACIONAL 21%(q)" 
			          @ 16,10 SAY "SMET (m)"
			          @ 18,10 SAY "PATINTER (p)"
			          @ 11,10 SAY "NACIONAL 1(8)%"
			          @ 19,10 say "iveco (v)"
			          @ 15,10 say "Introduzca tipo de cliente " get wmodalidad
                      read
                      wmodalidad=lower(wmodalidad)
                      do case
                          CASE wmodalidad="k"
                                 DO diezporc
	                      CASE wmodalidad="q"
	                             DO nacnuev21
	                      CASE WMODALIDAD="m"
	                             DO NACNUEV21SMET
	                      CASE wmodalidad="p"
	                             DO nacnuev21patinter
	                      case wmodalidad="a"
	                            *SIN IVA Y CON KILOMETROS
	                            do arcese
	                      case wmodalidad="e"
	                      *SIN IVA
	                            ****CON EXTRANJERO NO FUNCIONA
	                            do DEFUERA
	                                                                             
	                      case wmodalidad="n"
	                      *CON IVA
	                            do nacional
	                      case wmodalidad="o"
	                            do facturanormal
	                      CASE WMODALIDAD="r"
	                            DO SIETEPORC
	                      CASE wmodalidad="8"
	                            DO nacnuev 
	                      CASE wmodalidad="v"
	                            DO nacnuev21iveco
	                      otherwise


                      endcase
                 else

                      do case
      
                          CASE wmodalidad="10"
                                 DO diezporc
	                      CASE wmodalidad="21"
	                             DO nacnuev21
                          CASE WMODALIDAD="m"
                                 DO NACNUEV21SMET
                                 
                          CASE WMODALIDAD="p"
                                 DO NACNUEV21PATINTER      
	                      case wmodalidad="a"
	                            *SIN IVA Y CON KILOMETROS
	                            do arcese
	                      case wmodalidad="e"
	                            *SIN IVA
	                            ****CON EXTRANJERO NO FUNCIONA
	                            do DEFUERA
	                            ******* LINEA 1280
	                      case wmodalidad="n"
	                      *CON IVA
	                            do nacional
	                      case wmodalidad="o"

	                            do FACTURAnormal
	                      case wmodalidad="r"
	                            do sieteporc
	                      CASE wmodalidad="8"
	                            DO nacnuev 
	                      case wmodalidad="c"
	                            ********* LINEA 2297
	                            do comision
	                            
	                            
	                     CASE wmodalidad="v"
	                            
	                            
	                            DO nacnuev21iveco
	                      
	                      
	                    otherwise


                      endcase
                 endif
                 SET PRINT OFF
                 set printer to
                 
                 case respuesta="c"
                   
                 otherwise
                   
                 endcase

       ELSE
          ********la factura no esta en los ficheros
          ****************************************
                      clear
                       store space(40) to wNOMBRE
                       STORE SPACE(10) TO WNIF
                       *STORE SPACE(15) TO wempresa
                       @ 2,2 say "Introduzca nombre de empresa: " get wnombre
                       @ 3,2 SAY "NIF:    " GET WNIF
                       *@ 4,2 say "Nombre de empresa emisora si no es Cabrero " get wempresa
                       READ
                       *WEMPRESA=UPPER(WEMPRESA)
       IF WNIF=" "
            use CLIENTES index innombre,inclave,innif
                       if wnombre=" "
                           wnombre="A"
                       endif
                       wnombre=upper(wnombre)
                       wnombre=trim(wnombre)
                       SEEK wnombre
          ELSE
            USE clientes INDEX INNIF,innombre,INCLAVE
            WNIF=UPPER(WNIF)
            WNIF=TRIM(WNIF)
            SEEK WNIF
       ENDIF
         
         USAR=.F.
         
         IF USAR
         
          use clientes index innombre,inclave,innif
          clear
          store space(40) to wnombre
          @ 6,10 say "Introduzca nombre de empresa" get wnombre
          read
          WNOMBRE=UPPER(WNOMBRE)
          if wnombre=" "
              wnombre="A"
          endif
          wprueba=trim(wnombre)
          seek wprueba
         
         ENDIF
         
         
         
          browse fields nombre,POBLACION,NIF NOMENU NOAPPEND noedit nodelete
          encontrado="n"
          *@ 7,20 say "encontrado s/n " get encontrado

          *read
          
          *ENCONTRADO=LOWER(ENCONTRADO)
          encontrado=STR(MESSAGEBOX("encontrado s/n",4),1)
          if encontrado = "7"
                 ************************ el cliente no se encuentra en los ficheros



                 store space(40) to wnombre
                 store space(40) to wdireccion
                 store space(6)  to wcodpostal
                 store space(30) to wpoblacion
                 store space(10) to wprovincia
                 store space(10) to wpais
                 store space(16) to wnif
                 store space(10) to wclave
                 store space(40) to wdepartamen
                 store space(40) to wpoligono
                 store space(16) to wtelefono
                 store space(10) to wapdo
                 store space(10) to wcontacto
                 store space(12) to wactividad
                 store space(50) to wformapago
                 STORE SPACE(15) TO wempresa
                 clear
                 @ 6,5 say  "Nombre:        " get wnombre
                 @ 7,5 say  "Direccion:     " get wdireccion
                 @ 8,5 say  "Codigo Postal: " get wcodpostal
                 @ 9,5 say  "Poblacion:     " get wpoblacion
                 @ 10,5 say "Provincia:     " get wprovincia
                 @ 11,5 say "Pais:          " get wpais
                 @ 12,5 say "Nif:           " get wnif
                 @ 13,5 say "Clave:         " get wclave
                 @ 14,5 say "Departamento:  " get wdepartamen
                 @ 15,5 say "Poligono:      " get wpoligono
                 @ 16,5 say "Telefono:      " get wtelefono
                 @ 17,5 say "Apartado:      " get wapdo
                 @ 18,5 say "Contacto:      " get wcontacto
                 @ 19,5 say "Actividad:     " get wactividad
                 @ 20,5 say "Forma de pago: " get wformapago
                 read
                 wnformapago=wformapago
                 IF WCLAVE=" "
                 CLEAR

                 @ 13,5 say "Es necesaria la clave: " get wclave
                 READ
                 ENDIF
                 append blank
                 replace nombre with wnombre
                 replace direccion with wdireccion
                 replace codpostal with wcodpostal
                 replace poblacion with wpoblacion
                 replace provincia with wprovincia
                 replace pais with wpais
                 replace nif with wnif
                 replace clave with wclave
                 replace departamen with wdepartamen
                 replace poligono with wpoligono
                 replace telefono with wtelefono
                 replace apdo with wapdo
                 replace contacto with wcontacto
                 replace actividad with wactividad
                 replace formapago with wformapago
                 replace empresa WITH wempresa 
            else

                 *******SI EL CLIENTE YA ESTA EN EL FICHERO
                 clear
                 WFORMAPAGO=FORMAPAGO
                 @ 6,6 say "Modificar o añadir forma de pago: "
                 @ 7,6 say " " get wformapago
                 read
                 wnformapago=wformapago
                 REPLACE FORMAPAGO WITH WNFORMAPAGO
                 wnombre    = nombre
                 wdireccion = direccion
                 wpoblacion = poblacion
                 wprovincia = provincia
                 wcodpostal = codpostal
                 wpais      = pais
                 wnif       = nif
                 wclave     = clave
                 wformapago = formapago
                 wpoligono  = poligono
                 WAPDO      = APDO
           endif
           CLEAR
           fechaenv=ctod("  -  -  ")
           fechafac=CTOD("  -  -  ")
           @ 8,20  say "Fecha factura:    " get fechafac
           @ 10,20 say "Fecha envio:      " get fechaenv

           read
           *********************************
           ***********ELECCION DE MODALIDAD
           *******************************
           wmodalidad="n"
           clear
           @ 1,1 TO 24,79 DOUBLE
           @ 4,9 TO 21,70
           @ 3,10 say "Cliente: " +wnombre
     
           @ 5,10 SAY "con (c)omision"
           @ 7,10 say "n(o)rmal"
           @ 9,10 say "(a)rcese "
           @ 11,10 say "(e)xtranjero"
           *@ 14,10 say "(n)acional 16%"
           @ 15,10 SAY "8%(r)" 
           @ 14,10 SAY "NACIONAL 1(8)%"
           @ 12,10 SAY "REDUCIDO 10%(k)"
		   @ 13,10 SAY "NACIONAL 21%(q)"
           @ 16,10 SAY "SMET (m)"
           @ 18,10 say "PATINTER (p)"
           @ 17,10 SAY "(S)eguir"
           @ 19,10 say "Intoduzca tipo de cliente: " get wmodalidad
           @ 20,10 say "iveco (v)" 
           read
           wmodalidad=lower(wmodalidad )
           do case


               CASE wmodalidad="k"
                    DO diezporc
	           CASE wmodalidad="q"
	                DO nacnuev21

               CASE WMODALIDAD="m"
                    DO NACNUEV21SMET
               CASE wmodalidad="p"
                    DO nacnuev21patinter

	           case wmodalidad="a"
	                *SIN IVA Y CON KILOMETROS
	                do arcese
	           case wmodalidad="e"
	                *SIN IVA
	                do DEFUERA
	           case wmodalidad="n"
	                *CON IVA
	                do nacional
	           case wmodalidad="o"
	                *nacional sin origen ni destino
	                do FACTURAnormal
	           case wmodalidad="c"
	                do comision
	           CASE WMODALIDAD="r"
	                DO SIETEPORC
	           CASE wmodalidad="8"
	           
	                DO nacnuev 
	                
	                
	           CASE wmodalidad="v"
	                DO nacnuev21iveco
	                
	                
	           otherwise
	                *do nacional
           endcase
           SET PRINT OFF
           set printer to
           



     endif
     ********DEL PROGRAMA PRINCIPAL menu facturacion

case vmenu="p"
        use facturas index infac,inclife

        store space(5) to nfactura
        clear
        @ 6,20 say "Introduce factura: " get nfactura
        read
        if nfactura= " "
              nfactura="0"
        endif
        seek nfactura
        browse fields FECHA,factura,pagado,importe,moneda FREEZE PAGADO nomenu noappend nodelete

case vmenu="x"
         clear
         set talk on
         close indexes
         close databases
         set exclusive on
         use facturas index infac,inclife 
         reindex

         use clientes index inclave,innombre,innif 
         reindex

         use concepto index infacfec,innumero,in1ref,in1cmr 
         reindex
         CLOSE INDEX 
         CLOSE DATABASES
         set exclusive off
         set talk off


case vmenu="l"
         RESP ="c"
         MES=1
         ANO=2005
         CLEAR
         @ 10,10 SAY "LISTADO POR (C)LIENTES O POR (F)ACTURA" GET RESP
         @ 12,10 SAY "MES EN NUMERO                         " GET MES
         @ 14,10 SAY "AÑO                                   " GET ANO
         READ
         RESP=LOWER(RESP)
         DO CASE
               CASE RESP="c"
                   DO LISTCLI WITH MES,ANO
               CASE RESP="f"
                   DO LISTFAC WITH MES,ANO
        ENDCASE
case vmenu="r"
        do resulcam
case vmenu="t"
        mes=1
        ANO=2005
        clear
        @ 10,10 say "Listado de todas las facturas del mes"
        @ 12,10 say "Introduzca el mes                    " get mes
        @ 14,10 SAY "Introduzca el año                    " get ano
        read
        do tlistfac with mes,ANO
case vmenu=="c"

       **********LINEA 2347
       salida="f"
       do consulcli with salida
case vmenu="s"
       CAPSLOCK(WMAYUS)
       
       clear
       set talk on
       _screen.Caption="Microsoft Visual FoxPro"
       close index
       close databases
       set print off
       set printer to
       cancel
       
case vmenu="q"
       CAPSLOCK(WMAYUS)
       _screen.Caption="Microsoft Visual FoxPro"
       clear
       set talk on
       
       close index
       close databases
       set print off
       set printer to
       *Pop MENU _MSYSMENU
       *Set SYSMENU ON
       quit
case vmenu="u"
       store space(10) to wnum
       use concepto index in1ref
       clear
       @ 5,5 say "Introduce referencia: " get wnum
       read

       seek wnum
       WNUMFAC=NUMFAC
       USE FACTURAS INDEX INFAC
       seek(wnumfac)
       @ 10,5 say "Factura numero: " +Wnumfac
       @ 11,5 SAY "Fecha:          " +dtoc(fecha)
       @ 12,5 say "Cliente:        " +cliente
       ?
       ?
       ?
       ?
       ?
       WAIT WINDOW
       use
CASE VMENU="h"
       store space(10) to wcmr
       use concepto index in1cmr
       clear
       @ 5,5 say "Introduce nº CMR: " get wcmr
       read

       seek wcmr
       WNUMFAC=NUMFAC
       USE FACTURAS INDEX INFAC
       seek(wnumfac)
       @ 10,5 say "Factura numero: " +Wnumfac
       @ 11,5 SAY "Fecha:          " +dtoc(fecha)
       @ 12,5 say "Cliente:        " +cliente
       ?
       ?
       ?
       ?
       ?
       WAIT WINDOW
       use
case vmenu="m"
       do mancli
case vmenu="a"
       mes=1
        ANO=2005
        clear
        @ 10,10 say "Listado de todas las facturas del mes"
        @ 12,10 say "Introduzca el mes                    " get mes
        @ 14,10 SAY "Introduzca el año                    " get ano
        read
        do AGUADO with mes,ANO
CASE vmenu="e"      
        DO etiquetas
CASE vmenu="g"
        DO agencia
        
CASE vmenu=="cn"
        DO facturacion        

otherwise
       clear
       close index
       close databases
       set print off
       set printer to
       

endcase

enddo


**********************************
************FACTURA NACIONAL
***********************************
PROCEDURE NACIONAL
*********ENTRADA DE CONCEPTOS

if respuesta<>"i"  && es decir la factura es nueva
wmodalidad="n"

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
store space(10) to wreferencia
wpreckilom=0
wimporte=0
wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1
visor=.f. && para comprobar que no se graben registros en blanco
do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       store space(15) to worigen2
       store space(15) to wdestino1

       store space(15) to wdestino2
       store space(15) to wdestino3
       store space(15) to wdestino4
       store space(15) to wdestino5



       store space(10) to wreferencia
       STORE SPACE(10) TO WCMR
       wpreckilom=0
       wimporte=0
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas
       a="s"
       *DO FORM visprogfac_introducciondatosfactura


       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wmODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "Destino 5:        " get wdestino5
       @ 15,20 say "Nº Referencia:    " get wreferencia
       @ 15,60 SAY "CMR:  " GET WCMR
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte  PICTURE "999,999,999.99"
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Notas:            " get wnotas
       read
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -  "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       replace origen2    with worigen2
       replace destino1   with wdestino1

       replace destino2   with wdestino2
       replace destino3   with wdestino3
       replace destino4   with wdestino4
       replace destino5   with wdestino5
       replace referencia with wreferencia
       REPLACE CMR        WITH WCMR
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas
       
       visor=.t.
       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    a="s"
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    read
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo
if .not.visor
         clear
         @ 10,10 say "No se ha grabado ningun concepto por"
         @ 11,10 say "no haber introducido ninguna fecha."

else
clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*16
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with  wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


**IMPRESION DE NACIONAL
prueba=.F.

IF (.not. prueba) .and. wempresa<>"AGENCIA CABRERO" && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES



IF RESPUESTA="i"
       VISOR=.t.  
ENDIF
if visor
	
	XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file factura.txt
	    
	   
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
	SET CENTURY OFF
	*set print font "courier new",11
	*SET PRINTER FONT "lucida console",11
	SET PRINTER FONT "monospac821 bt",11
	@ 10,8 say wnombre
	@ 11,8 say wdireccion
	IF WAPDO<>" "
	         @ 12,8 say wapdo 
	ENDIF

	if wpoligono<>" "
	         @ 13,8 say wpoligono 
	endif
	@ 14,8 say wcodpostal+" "+left(wpoblacion,25)
	if wpais<>"ESPAÑA"
	   @ 15,8 say wprovincia+" "+wpais 
	ELSE
	   @ 15,8 say wprovincia
	endif

	@ 17,56 say "NIF "+wnif
	if wpoligono= " "
	endif

	IF WAPDO=" "

	ENDIF

	
	SET CENTURY ON
	@ 20,14 say wnumfac
	@ 20,67 say fechafac
	SET CENTURY OFF

	@ 23,4 say "FORMA DE PAGO: "+wnformapago

	*set print font "courier new",10
	SET PRINTER FONT "monospac821 bt",10
	
	*SET PRINTER FONT "lucida console",10
	********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
	use concepto index infacfec,innumero,in1ref

	seek wnumfac
	if respuesta="i"
	       finaltotal=0
	endif
	indice=0
	@ 29,5 say ""

	do  while numfac=Wnumfac

	     IF MONEDA="EUR"

	       resultado=transform(importe,"999,999,999.99")
	       ELSE




	       resultado=transform(importe,"999,999,999")

	     ENDIF
	      
	       IF MONEDA="EUR"
	              RESULTADO=RIGHT(RESULTADO,9)
	       ENDIF

	       @ prow()+1,4 say dtoc(fecha)
	       @ prow(),13 say left(matricula,10)
	       @ prow(),25 say left(origen1,15)+"-"+left(destino1,15)
	       @ prow(),79 say resultado+" "+moneda 
	       if (origen2<>" ").or.(destino2<>" ")
	         @ prow()+1,25 say left(origen2,15)+"-"+destino2       
	       endif
	       if destino3<>" "
	           @ prow()+1,42 say destino3      
	       endif
	       if destino4<>" "
	           *?replicate(" ",43),destino4
	           @ prow()+1,42 say destino4     
	       endif
	       if destino5<>" "
	           @ prow()+1,42 say destino5 
	       endif
	       if notas<>" "
	           @ prow()+1,25 say notas      
	       endif
	       indice=indice+1
	       wmoneda=moneda
	       if respuesta="i"
	                finaltotal=finaltotal+importe
	       endif
	       skip

	enddo
	?
	if respuesta<>"i"
	       finaltotal=finaltotal-iva
	endif
	if indice>1

		 IF WMONEDA="EUR"

		       resultadototal=transform(finaltotal,"999,999,999.99")
		    ELSE
		       resultadototal=transform(finaltotal,"999,999,999")
		 ENDIF
		      
		 IF WMONEDA="EUR"
		        RESULTADOTOTAL=RIGHT(RESULTADOTOTAL,9)

		 ENDIF
		     @ prow()+1,79 say "---------"
		     @ prow()+1,79 say resultadototal+" "+wmoneda
	endif
	if respuesta="i"
	       iva=0
	       iva=(finaltotal)/100*16
	endif
	IF WMONEDA="EUR"
	         resultadoiva=transform(iva,"999,999,999.99")
	     ELSE
	         resultadoiva=transform(iva,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOIVA=RIGHT(RESULTADOIVA,9)
	ENDIF

	@ prow()+2,65 say "16% IVA"
	@ prow(),79 say resultadoiva+" "+wmoneda
	@ prow()+1,79 say "---------"
	if respuesta="i"
	    finaltotal=finaltotal+iva
	endif
	if respuesta<>"i"
	       finaltotal=finaltotal+iva
	endif
	***************************
	******* tratamiento de euros
	*****************************
	euros=finaltotal/166.386
	reuros=transform(euros,"999,999,999.99")

	      




	************************************
	IF WMONEDA="EUR"

	        resultadofinal=transform(finaltotal,"999,999,999.99")
	     ELSE
	        resultadofinal=transform(finaltotal,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOFINAL=RIGHT(RESULTADOFINAL,9)
	ENDIF

	@ prow()+1,67 say "TOTAL.."
	@ prow(),79 say resultadofinal+" "+wmoneda
	IF WMONEDA<>"EUR"
	
	   @ prow()+1,67 say reuros+"EUR"
	ENDIF
	*******************+
	@ prow()+1,79 say "========="
	*******impresion de notas

	    
	       @ prow()+3,25 say wnota1
	       @ prow()+1,25 say wnota2
	       @ prow()+1,25 say wnota3
	       @ prow()+1,25 say wnota4
	       @ prow()+1,25 say wnota5 
	       @ prow()+1,25 say wnota6
	       @ prow()+1,25 say wnota7
  	
  do case
   	case xx="I"
	   set print off
	   set printer to
	   set device to screen
    case xx="P"
       set device to screen
       modify file factura.txt noedit
        
       delete file factura.txt
  endcase

	SET CENTURY ON
ENDIF





ELSE && de if .not. prueba
 
IF WEMPRESA="AGENCIA CABRERO"
  DO AGENCIACABRERO
ELSE && DE AGENCIA CABRERO DENTRO DE  if .not. prueba
 
 
CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasnacional.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on

ENDIF && DE AGENCIA CABRERO

ENDIF && de if .not. prueba


WAIT WINDOW
RETURN
***********************************************************************
***********************************************************************




**********************************
************FACTURA NACIONAL 18%
***********************************
PROCEDURE NACnuev
*********ENTRADA DE CONCEPTOS

if respuesta<>"i"  && es decir la factura es nueva
wmodalidad="8"

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
store space(10) to wreferencia
wpreckilom=0
wimporte=0
wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1
visor=.f. && para comprobar que no se graben registros en blanco
do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       store space(15) to worigen2
       store space(15) to wdestino1

       store space(15) to wdestino2
       store space(15) to wdestino3
       store space(15) to wdestino4
       store space(15) to wdestino5



       store space(10) to wreferencia
       STORE SPACE(10) TO WCMR
       wpreckilom=0
       wimporte=0
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas
       a="s"
       DO FORM visprogfac_introducciondatosfactura
       SI=.F.
       IF SI  && SE EJECUTA EL CODIGO

       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wmODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "Destino 5:        " get wdestino5
       @ 15,20 say "Nº Referencia:    " get wreferencia
       @ 15,60 SAY "CMR:  " GET WCMR
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte  PICTURE "999,999,999.99"
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Notas:            " get wnotas
       read
       ENDIF && DE SI
       
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -    "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       replace origen2    with worigen2
       replace destino1   with wdestino1

       replace destino2   with wdestino2
       replace destino3   with wdestino3
       replace destino4   with wdestino4
       replace destino5   with wdestino5
       replace referencia with wreferencia
       REPLACE CMR        WITH WCMR
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas
       
       visor=.t.
       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    
    IF SI && SE EJECUTA EL CODIGO
    
    a="s"
    
    
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    READ
    ENDIF && DE SI
    
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo
if .not.visor
         clear
         @ 10,10 say "No se ha grabado ningun concepto por"
         @ 11,10 say "no haber introducido ninguna fecha."

else
clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*18
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with  wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


**IMPRESION DE NACIONAL
prueba=.F.

*IF .not. prueba && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES
IF (.not. prueba) .and. wempresa<>"AGENCIA CABRERO" && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES


IF RESPUESTA="i"
       VISOR=.t.  
ENDIF
if visor
	
	XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file factura.txt
	    
	   
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
	SET CENTURY OFF
	*set print font "courier new",11
	*SET PRINTER FONT "lucida console",11
	SET PRINTER FONT "monospac821 bt",11
	@ 10,46 say wnombre
	@ 11,46 say wdireccion
	IF WAPDO<>" "
	         @ 12,46 say wapdo 
	ENDIF

	if wpoligono<>" "
	         @ 13,46 say wpoligono 
	endif
	@ 14,46 say wcodpostal+" "+left(wpoblacion,25)
	if wpais<>"ESPAÑA"
	   @ 15,46 say wprovincia+" "+wpais 
	ELSE
	   @ 15,46 say wprovincia
	endif

	@ 17,56 say "NIF "+wnif
	if wpoligono= " "
	endif

	IF WAPDO=" "

	ENDIF

	
	SET CENTURY ON
	@ 20,14 say wnumfac
	@ 20,67 say fechafac
	SET CENTURY OFF

	@ 23,4 say "FORMA DE PAGO: "+wnformapago

	*set print font "courier new",10
	SET PRINTER FONT "monospac821 bt",10
	
	*SET PRINTER FONT "lucida console",10
	********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
	use concepto index infacfec,innumero,in1ref

	seek wnumfac
	if respuesta="i"
	       finaltotal=0
	endif
	indice=0
	@ 29,5 say ""

	do  while numfac=Wnumfac

	     IF MONEDA="EUR"

	       resultado=transform(importe,"999,999,999.99")
	       ELSE




	       resultado=transform(importe,"999,999,999")

	     ENDIF
	      
	       IF MONEDA="EUR"
	              RESULTADO=RIGHT(RESULTADO,9)
	       ENDIF

	       @ prow()+1,4 say dtoc(fecha)
	       @ prow(),13 say left(matricula,10)
	       @ prow(),25 say left(origen1,15)+"-"+left(destino1,15)
	       @ prow(),79 say resultado+" "+moneda 
	       if (origen2<>" ").or.(destino2<>" ")
	         @ prow()+1,25 say left(origen2,15)+"-"+destino2       
	       endif
	       if destino3<>" "
	           @ prow()+1,42 say destino3      
	       endif
	       if destino4<>" "
	           *?replicate(" ",43),destino4
	           @ prow()+1,42 say destino4     
	       endif
	       if destino5<>" "
	           @ prow()+1,42 say destino5 
	       endif
	       if notas<>" "
	           @ prow()+1,25 say notas      
	       endif
	       indice=indice+1
	       wmoneda=moneda
	       if respuesta="i"
	                finaltotal=finaltotal+importe
	       endif
	       skip

	enddo
	?
	if respuesta<>"i"
	       finaltotal=finaltotal-iva
	endif
	if indice>1

		 IF WMONEDA="EUR"

		       resultadototal=transform(finaltotal,"999,999,999.99")
		    ELSE
		       resultadototal=transform(finaltotal,"999,999,999")
		 ENDIF
		      
		 IF WMONEDA="EUR"
		        RESULTADOTOTAL=RIGHT(RESULTADOTOTAL,9)

		 ENDIF
		     @ prow()+1,79 say "---------"
		     @ prow()+1,79 say resultadototal+" "+wmoneda
	endif
	if respuesta="i"
	       iva=0
	       iva=(finaltotal)/100*18
	endif
	IF WMONEDA="EUR"
	         resultadoiva=transform(iva,"999,999,999.99")
	     ELSE
	         resultadoiva=transform(iva,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOIVA=RIGHT(RESULTADOIVA,9)
	ENDIF

	@ prow()+2,65 say "18% IVA"
	@ prow(),79 say resultadoiva+" "+wmoneda
	@ prow()+1,79 say "---------"
	if respuesta="i"
	    finaltotal=finaltotal+iva
	endif
	if respuesta<>"i"
	       finaltotal=finaltotal+iva
	endif
	***************************
	******* tratamiento de euros
	*****************************
	euros=finaltotal/166.386
	reuros=transform(euros,"999,999,999.99")

	      




	************************************
	IF WMONEDA="EUR"

	        resultadofinal=transform(finaltotal,"999,999,999.99")
	     ELSE
	        resultadofinal=transform(finaltotal,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOFINAL=RIGHT(RESULTADOFINAL,9)
	ENDIF

	@ prow()+1,67 say "TOTAL.."
	@ prow(),79 say resultadofinal+" "+wmoneda
	IF WMONEDA<>"EUR"
	
	   @ prow()+1,67 say reuros+"EUR"
	ENDIF
	*******************+
	@ prow()+1,79 say "========="
	*******impresion de notas

	    
	       @ prow()+3,25 say wnota1
	       @ prow()+1,25 say wnota2
	       @ prow()+1,25 say wnota3
	       @ prow()+1,25 say wnota4
	       @ prow()+1,25 say wnota5 
	       @ prow()+1,25 say wnota6
	       @ prow()+1,25 say wnota7
  	
  do case
   	case xx="I"
	   set print off
	   set printer to
	   set device to screen
    case xx="P"
       set device to screen
       modify file factura.txt noedit
        
       delete file factura.txt
  endcase

	SET CENTURY ON
ENDIF





ELSE && de if .not. prueba


IF WEMPRESA="AGENCIA CABRERO"
  DO AGENCIACABRERO
ELSE && DE AGENCIA CABRERO DENTRO DE  if .not. prueba



CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasnacional.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on

ENDIF && DE AGENCIA CABRERO

ENDIF && de if .not. prueba


WAIT WINDOW
RETURN
***********************************************************************
***********************************************************************




**********************************
************FACTURA NACIONAL 21%
***********************************
PROCEDURE NACnuev21
*********ENTRADA DE CONCEPTOS

if respuesta<>"i"  && es decir la factura es nueva
wmodalidad="21"

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
store space(10) to wreferencia
wpreckilom=0
wimporte=0
wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1
visor=.f. && para comprobar que no se graben registros en blanco
do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       store space(15) to worigen2
       store space(15) to wdestino1

       store space(15) to wdestino2
       store space(15) to wdestino3
       store space(15) to wdestino4
       store space(15) to wdestino5



       store space(10) to wreferencia
       STORE SPACE(10) TO WCMR
       wpreckilom=0
       wimporte=0
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas
       store space(38) to wnotas2
       a="s"
       DO FORM visprogfac_introducciondatosfactura
       SI=.F.
       IF SI  && SE EJECUTA EL CODIGO

       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wmODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "Destino 5:        " get wdestino5
       @ 15,20 say "Nº Referencia:    " get wreferencia
       @ 15,60 SAY "CMR:  " GET WCMR
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte  PICTURE "999,999,999.99"
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Notas:            " get wnotas
       read
       ENDIF && DE SI
       
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -    "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       replace origen2    with worigen2
       replace destino1   with wdestino1

       replace destino2   with wdestino2
       replace destino3   with wdestino3
       replace destino4   with wdestino4
       replace destino5   with wdestino5
       replace referencia with wreferencia
       REPLACE CMR        WITH WCMR
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas
       replace notas2     WITH wnotas2
       visor=.t.
       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    
    IF SI && SE EJECUTA EL CODIGO 
    
    a="s"
    
    
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    READ
    ENDIF && DE SI
    
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo
if .not.visor
         clear
         @ 10,10 say "No se ha grabado ningun concepto por"
         @ 11,10 say "no haber introducido ninguna fecha."

else
clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*21
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with  wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


**IMPRESION DE NACIONAL
prueba=.F.

*IF .not. prueba && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES
IF (.not. prueba) .and. wempresa<>"AGENCIA CABRERO" && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES


IF RESPUESTA="i"
       VISOR=.t.  
ENDIF
if visor
	
	XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file factura.txt
	    
	   
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
	SET CENTURY OFF
	*set print font "courier new",11
	*SET PRINTER FONT "lucida console",11
	SET PRINTER FONT "monospac821 bt",11
	@ 10,46 say wnombre
	@ 11,46 say wdireccion
	IF WAPDO<>" "
	         @ 12,46 say wapdo 
	ENDIF

	if wpoligono<>" "
	         @ 13,46 say wpoligono 
	endif
	@ 14,46 say wcodpostal+" "+left(wpoblacion,25)
	if wpais<>"ESPAÑA"
	   @ 15,46 say wprovincia+" "+wpais 
	ELSE
	   @ 15,46 say wprovincia
	endif

	@ 17,56 say "NIF "+wnif
	if wpoligono= " "
	endif

	IF WAPDO=" "

	ENDIF

	
	SET CENTURY ON
	
	IF WNUMFAC="00000"
	   @ 20,14 SAY "PROFORMA"
	ELSE
	@ 20,14 say wnumfac
	ENDIF
	
	
	@ 20,67 say fechafac
	SET CENTURY OFF

	@ 23,4 say "FORMA DE PAGO: "+wnformapago

	*set print font "courier new",10
	SET PRINTER FONT "monospac821 bt",10
	
	*SET PRINTER FONT "lucida console",10
	********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
	use concepto index infacfec,innumero,in1ref

	seek wnumfac
	if respuesta="i"
	       finaltotal=0
	endif
	indice=0
	@ 29,5 say ""

	do  while numfac=Wnumfac

	     IF MONEDA="EUR"

	       resultado=transform(importe,"999,999,999.99")
	       ELSE




	       resultado=transform(importe,"999,999,999")

	     ENDIF
	      
	       IF MONEDA="EUR"
	              RESULTADO=RIGHT(RESULTADO,9)
	       ENDIF

	       @ prow()+1,4 say dtoc(fecha)
	       @ prow(),13 say left(matricula,10)
	       @ prow(),25 say left(origen1,15)+"-"+left(destino1,15)
	       @ prow(),79 say resultado+" "+moneda 
	       if (origen2<>" ").or.(destino2<>" ")
	         @ prow()+1,25 say left(origen2,15)+"-"+destino2       
	       endif
	       if destino3<>" "
	           @ prow()+1,42 say destino3      
	       endif
	       if destino4<>" "
	           *?replicate(" ",43),destino4
	           @ prow()+1,42 say destino4     
	       endif
	       if destino5<>" "
	           @ prow()+1,42 say destino5 
	       endif
	       if notas<>" "
	           @ prow()+1,25 say notas      
	       ENDIF
	       
	       if notas2<>" "
	           @ prow()+1,25 say notas2      
	       endif
	       indice=indice+1
	       wmoneda=moneda
	       if respuesta="i"
	                finaltotal=finaltotal+importe
	       endif
	       skip

	enddo
	?
	if respuesta<>"i"
	       finaltotal=finaltotal-iva
	endif
	if indice>1

		 IF WMONEDA="EUR"

		       resultadototal=transform(finaltotal,"999,999,999.99")
		    ELSE
		       resultadototal=transform(finaltotal,"999,999,999")
		 ENDIF
		      
		 IF WMONEDA="EUR"
		        RESULTADOTOTAL=RIGHT(RESULTADOTOTAL,9)

		 ENDIF
		     @ prow()+1,79 say "---------"
		     @ prow()+1,79 say resultadototal+" "+wmoneda
	endif
	if respuesta="i"
	       iva=0
	       iva=(finaltotal)/100*21
	endif
	IF WMONEDA="EUR"
	         resultadoiva=transform(iva,"999,999,999.99")
	     ELSE
	         resultadoiva=transform(iva,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOIVA=RIGHT(RESULTADOIVA,9)
	ENDIF

	@ prow()+2,65 say "21% IVA"
	@ prow(),79 say resultadoiva+" "+wmoneda
	@ prow()+1,79 say "---------"
	if respuesta="i"
	    finaltotal=finaltotal+iva
	endif
	if respuesta<>"i"
	       finaltotal=finaltotal+iva
	endif
	***************************
	******* tratamiento de euros
	*****************************
	euros=finaltotal/166.386
	reuros=transform(euros,"999,999,999.99")

	      




	************************************
	IF WMONEDA="EUR"

	        resultadofinal=transform(finaltotal,"999,999,999.99")
	     ELSE
	        resultadofinal=transform(finaltotal,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOFINAL=RIGHT(RESULTADOFINAL,9)
	ENDIF

	@ prow()+1,67 say "TOTAL.."
	@ prow(),79 say resultadofinal+" "+wmoneda
	IF WMONEDA<>"EUR"
	
	   @ prow()+1,67 say reuros+"EUR"
	ENDIF
	*******************+
	@ prow()+1,79 say "========="
	*******impresion de notas

	    
	       @ prow()+3,25 say wnota1
	       @ prow()+1,25 say wnota2
	       @ prow()+1,25 say wnota3
	       @ prow()+1,25 say wnota4
	       @ prow()+1,25 say wnota5 
	       @ prow()+1,25 say wnota6
	       @ prow()+1,25 say wnota7
  	
  do case
   	case xx="I"
	   set print off
	   set printer to
	   set device to screen
    case xx="P"
       set device to screen
       modify file factura.txt noedit
        
       delete file factura.txt
  endcase

	SET CENTURY ON
ENDIF





ELSE && de if .not. prueba


IF WEMPRESA="AGENCIA CABRERO"
  DO AGENCIACABRERO
ELSE && DE AGENCIA CABRERO DENTRO DE  if .not. prueba



CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasnacional.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on

ENDIF && DE AGENCIA CABRERO

ENDIF && de if .not. prueba


WAIT WINDOW
RETURN
***********************************************************************
***********************************************************************
*********FIN DE PROCEDURE NACNUEV21





**********************************
************FACTURA NACIONAL iveco 21%
***********************************
PROCEDURE NACnuev21iveco
*********ENTRADA DE CONCEPTOS

if respuesta<>"i"  && es decir la factura es nueva
wmodalidad="21"

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
store space(10) to wreferencia
wpreckilom=0
wimporte=0
wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1
visor=.f. && para comprobar que no se graben registros en blanco
do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       store space(15) to worigen2
       store space(15) to wdestino1

       store space(15) to wdestino2
       store space(15) to wdestino3
       store space(15) to wdestino4
       store space(15) to wdestino5



       store space(10) to wreferencia
       STORE SPACE(10) TO WCMR
       wpreckilom=0
       wimporte=0
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas
       store space(38) to wnotas2
       
       
        store space(38) to wnotas3
         store space(38) to wnotas4
          store space(38) to wnotas5
           store space(38) to wnotas6
       
       a="s"
       DO FORM visprogfac_introducciondatosfacturaiveco
       SI=.F.
       IF SI  && SE EJECUTA EL CODIGO

       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wmODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "Destino 5:        " get wdestino5
       @ 15,20 say "Nº Referencia:    " get wreferencia
       @ 15,60 SAY "CMR:  " GET WCMR
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte  PICTURE "999,999,999.99"
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Notas:            " get wnotas
       read
       ENDIF && DE SI
       
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -    "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       replace origen2    with worigen2
       replace destino1   with wdestino1

       replace destino2   with wdestino2
       replace destino3   with wdestino3
       replace destino4   with wdestino4
       replace destino5   with wdestino5
       replace referencia with wreferencia
       REPLACE CMR        WITH WCMR
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas
       replace notas2     WITH wnotas2
       replace notas3     WITH wnotas3
       replace notas4     WITH wnotas4
       replace notas5     WITH wnotas5
       replace notas6     WITH wnotas6
       
       
       
       
       
       
       visor=.t.
       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    
    IF SI && SE EJECUTA EL CODIGO 
    
    a="s"
    
    
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    READ
    ENDIF && DE SI
    
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo
if .not.visor
         clear
         @ 10,10 say "No se ha grabado ningun concepto por"
         @ 11,10 say "no haber introducido ninguna fecha."

else
clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*21
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with  wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


**IMPRESION DE NACIONAL



CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasnacional2.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on



WAIT WINDOW
RETURN
***********************************************************************
***********************************************************************
*********FIN DE PROCEDURE NACNUEV21IVECO





**********************************
************FACTURA NACIONAL 21% SMET
***********************************
PROCEDURE NACnuev21SMET
*********ENTRADA DE CONCEPTOS

if respuesta<>"i"  && es decir la factura es nueva
wmodalidad="m"

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
store space(10) to wreferencia
wpreckilom=0
WKILOMETROS=0
WVACLLENO="vacio"
STORE SPACE(10) TO wnumviaje

wimporte=0
wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1
visor=.f. && para comprobar que no se graben registros en blanco
do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       *store space(15) to worigen2
       store space(15) to wdestino1

       *store space(15) to wdestino2
       *store space(15) to wdestino3
       *store space(15) to wdestino4
       *store space(15) to wdestino5



       store space(10) to wreferencia
       STORE SPACE(10) TO WCMR
       wpreckilom=0
       WKILOMETROS=0
       WVACLLENO="vacio"
       STORE SPACE(10) TO wnumviaje
       wimporte=0
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas
       a="s"
       DO FORM visprogfac_introducciondatosfacturaSMET
       SI=.F.
       IF SI  && SE EJECUTA EL CODIGO

       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wmODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "Destino 5:        " get wdestino5
       @ 15,20 say "Nº Referencia:    " get wreferencia
       @ 15,60 SAY "CMR:  " GET WCMR
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte  PICTURE "999,999,999.99"
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Notas:            " get wnotas
       read
       ENDIF && DE SI
       
       
       wimporte=wkilometros*wpreckilom
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -    "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       *replace origen2    with worigen2
       replace destino1   with wdestino1
       REPLACE VACLLENO WITH WVACLLENO
       REPLACE KILOMETROS WITH WKILOMETROS
       replace numviaje WITH wnumviaje
       *replace destino2   with wdestino2
       *replace destino3   with wdestino3
       *replace destino4   with wdestino4
       *replace destino5   with wdestino5
       replace referencia with wreferencia
       REPLACE CMR        WITH WCMR
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas
       
       visor=.t.
       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    
    IF SI && SE EJECUTA EL CODIGO 
    
    a="s"
    
    
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    READ
    ENDIF && DE SI
    
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo
if .not.visor
         clear
         @ 10,10 say "No se ha grabado ningun concepto por"
         @ 11,10 say "no haber introducido ninguna fecha."

else
clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*21
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with  wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


**IMPRESION DE NACIONAL
prueba=.F.

*IF .not. prueba && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES
IF (.not. prueba) .and. wempresa<>"AGENCIA CABRERO" && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES


IF RESPUESTA="i"
       VISOR=.t.  
ENDIF
if visor
	
	XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file factura.txt
	    
	   
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
	SET CENTURY OFF
	*set print font "courier new",11
	*SET PRINTER FONT "lucida console",11
	SET PRINTER FONT "monospac821 bt",11
	@ 10,46 say wnombre
	@ 11,46 say wdireccion
	IF WAPDO<>" "
	         @ 12,46 say wapdo 
	ENDIF

	if wpoligono<>" "
	         @ 13,46 say wpoligono 
	endif
	@ 14,46 say wcodpostal+" "+left(wpoblacion,25)
	if wpais<>"ESPAÑA"
	   @ 15,46 say wprovincia+" "+wpais 
	ELSE
	   @ 15,46 say wprovincia
	endif

	@ 17,56 say "NIF "+wnif
	if wpoligono= " "
	endif

	IF WAPDO=" "

	ENDIF

	
	SET CENTURY ON
	
	IF WNUMFAC="00000"
	   @ 20,14 SAY "PROFORMA"
	ELSE
	@ 20,14 say wnumfac
	ENDIF
	
	
	@ 20,67 say fechafac
	SET CENTURY OFF

	@ 23,4 say "FORMA DE PAGO: "+wnformapago

	*set print font "courier new",10
	SET PRINTER FONT "monospac821 bt",10
	
	*SET PRINTER FONT "lucida console",10
	********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
	use concepto index infacfec,innumero,in1ref

	seek wnumfac
	if respuesta="i"
	       finaltotal=0
	endif
	indice=0
	@ 29,5 say ""

	do  while numfac=Wnumfac

	     IF MONEDA="EUR"

	       resultado=transform(importe,"999,999,999.99")
	       ELSE




	       resultado=transform(importe,"999,999,999")

	     ENDIF
	      
	       IF MONEDA="EUR"
	              RESULTADO=RIGHT(RESULTADO,9)
	       ENDIF

	       @ prow()+1,4 say dtoc(fecha)
	       @ prow(),13 say left(matricula,10)
	       @ prow(),25 say left(origen1,10)+"-"+left(destino1,10)+"Nº "+LEFT(numviaje,10)+vaclleno
	       *resultado=wkilometros*wpreckilom
	       
	       @ prow(),79 say resultado+" "+moneda 
	       *if (origen2<>" ").or.(destino2<>" ")
	         @ prow()+1,25 say STR(kilometros,5)+" km."+STR(preckilom,10,2)+" eur/km."       
	       *endif
	       *if destino3<>" "
	       *    @ prow()+1,42 say destino3      
	       *endif
	       *if destino4<>" "
	           *?replicate(" ",43),destino4
	       *    @ prow()+1,42 say destino4     
	       *endif
	       *if destino5<>" "
	       *    @ prow()+1,42 say destino5 
	       *endif
	       if notas<>" "
	           @ prow()+1,25 say notas      
	       endif
	       indice=indice+1
	       wmoneda=moneda
	       if respuesta="i"
	                finaltotal=finaltotal+importe
	       endif
	       skip

	enddo
	?
	if respuesta<>"i"
	       finaltotal=finaltotal-iva
	endif
	if indice>1

		 IF WMONEDA="EUR"

		       resultadototal=transform(finaltotal,"999,999,999.99")
		    ELSE
		       resultadototal=transform(finaltotal,"999,999,999")
		 ENDIF
		      
		 IF WMONEDA="EUR"
		        RESULTADOTOTAL=RIGHT(RESULTADOTOTAL,9)

		 ENDIF
		     @ prow()+1,79 say "---------"
		     @ prow()+1,79 say resultadototal+" "+wmoneda
	endif
	if respuesta="i"
	       iva=0
	       iva=(finaltotal)/100*21
	endif
	IF WMONEDA="EUR"
	         resultadoiva=transform(iva,"999,999,999.99")
	     ELSE
	         resultadoiva=transform(iva,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOIVA=RIGHT(RESULTADOIVA,9)
	ENDIF

	@ prow()+2,65 say "21% IVA"
	@ prow(),79 say resultadoiva+" "+wmoneda
	@ prow()+1,79 say "---------"
	if respuesta="i"
	    finaltotal=finaltotal+iva
	endif
	if respuesta<>"i"
	       finaltotal=finaltotal+iva
	endif
	***************************
	******* tratamiento de euros
	*****************************
	euros=finaltotal/166.386
	reuros=transform(euros,"999,999,999.99")

	      




	************************************
	IF WMONEDA="EUR"

	        resultadofinal=transform(finaltotal,"999,999,999.99")
	     ELSE
	        resultadofinal=transform(finaltotal,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOFINAL=RIGHT(RESULTADOFINAL,9)
	ENDIF

	@ prow()+1,67 say "TOTAL.."
	@ prow(),79 say resultadofinal+" "+wmoneda
	IF WMONEDA<>"EUR"
	
	   @ prow()+1,67 say reuros+"EUR"
	ENDIF
	*******************+
	@ prow()+1,79 say "========="
	*******impresion de notas

	    
	       @ prow()+3,25 say wnota1
	       @ prow()+1,25 say wnota2
	       @ prow()+1,25 say wnota3
	       @ prow()+1,25 say wnota4
	       @ prow()+1,25 say wnota5 
	       @ prow()+1,25 say wnota6
	       @ prow()+1,25 say wnota7
  	
  do case
   	case xx="I"
	   set print off
	   set printer to
	   set device to screen
    case xx="P"
       set device to screen
       modify file factura.txt noedit
        
       delete file factura.txt
  endcase

	SET CENTURY ON
ENDIF





ELSE && de if .not. prueba


IF WEMPRESA="AGENCIA CABRERO"
  DO AGENCIACABRERO
ELSE && DE AGENCIA CABRERO DENTRO DE  if .not. prueba



CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasnacional.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on

ENDIF && DE AGENCIA CABRERO

ENDIF && de if .not. prueba


WAIT WINDOW
RETURN
***********************************************************************
***********************************************************************
*********FIN DE PROCEDURE NACNUEV21smet





**********************************
************FACTURA NACIONAL 21% PATINTER
***********************************
PROCEDURE NACnuev21PATINTER
*********ENTRADA DE CONCEPTOS

if respuesta<>"i"  && es decir la factura es nueva
wmodalidad="m"

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
store space(10) to wreferencia
wpreckilom=0
WKILOMETROS=0
WVACLLENO="vacio"
STORE SPACE(10) TO wnumviaje

wimporte=0
wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1
visor=.f. && para comprobar que no se graben registros en blanco
do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       *store space(15) to worigen2
       store space(15) to wdestino1

       *store space(15) to wdestino2
       *store space(15) to wdestino3
       *store space(15) to wdestino4
       *store space(15) to wdestino5



       store space(10) to wreferencia
       STORE SPACE(10) TO WCMR
       wpreckilom=0
       WKILOMETROS=0
       WVACLLENO="vacio"
       STORE SPACE(10) TO wnumviaje
       wimporte=0
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas
       a="s"
       DO FORM visprogfac_introducciondatosfacturaSPATINTER
       SI=.F.
       IF SI  && SE EJECUTA EL CODIGO

       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wmODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "Destino 5:        " get wdestino5
       @ 15,20 say "Nº Referencia:    " get wreferencia
       @ 15,60 SAY "CMR:  " GET WCMR
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte  PICTURE "999,999,999.99"
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Notas:            " get wnotas
       read
       ENDIF && DE SI
       
       
       wimporte=wkilometros*wpreckilom
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -    "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       *replace origen2    with worigen2
       replace destino1   with wdestino1
       REPLACE VACLLENO WITH WVACLLENO
       REPLACE KILOMETROS WITH WKILOMETROS
       replace numviaje WITH wnumviaje
       *replace destino2   with wdestino2
       *replace destino3   with wdestino3
       *replace destino4   with wdestino4
       *replace destino5   with wdestino5
       replace referencia with wreferencia
       REPLACE CMR        WITH WCMR
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas
       
       visor=.t.
       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    
    IF SI && SE EJECUTA EL CODIGO 
    
    a="s"
    
    
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    READ
    ENDIF && DE SI
    
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo
if .not.visor
         clear
         @ 10,10 say "No se ha grabado ningun concepto por"
         @ 11,10 say "no haber introducido ninguna fecha."

else
clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*21
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with  wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


**IMPRESION DE NACIONAL
prueba=.F.

*IF .not. prueba && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES
IF (.not. prueba) .and. wempresa<>"AGENCIA CABRERO" && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES


IF RESPUESTA="i"
       VISOR=.t.  
ENDIF
if visor
	
	XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file factura.txt
	    
	   
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
	SET CENTURY OFF
	*set print font "courier new",11
	*SET PRINTER FONT "lucida console",11
	SET PRINTER FONT "monospac821 bt",11
	@ 10,46 say wnombre
	@ 11,46 say wdireccion
	IF WAPDO<>" "
	         @ 12,46 say wapdo 
	ENDIF

	if wpoligono<>" "
	         @ 13,46 say wpoligono 
	endif
	@ 14,46 say wcodpostal+" "+left(wpoblacion,25)
	if wpais<>"ESPAÑA"
	   @ 15,46 say wprovincia+" "+wpais 
	ELSE
	   @ 15,46 say wprovincia
	endif

	@ 17,56 say "NIF "+wnif
	if wpoligono= " "
	endif

	IF WAPDO=" "

	ENDIF

	
	SET CENTURY ON
	
	IF WNUMFAC="00000"
	   @ 20,14 SAY "PROFORMA"
	ELSE
	@ 20,14 say wnumfac
	ENDIF
	
	
	@ 20,67 say fechafac
	SET CENTURY OFF

	@ 23,4 say "FORMA DE PAGO: "+wnformapago

	*set print font "courier new",10
	SET PRINTER FONT "monospac821 bt",10
	
	*SET PRINTER FONT "lucida console",10
	********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
	use concepto index infacfec,innumero,in1ref

	seek wnumfac
	if respuesta="i"
	       finaltotal=0
	endif
	indice=0
	@ 29,5 say ""

	do  while numfac=Wnumfac

	     IF MONEDA="EUR"

	       resultado=transform(importe,"999,999,999.99")
	       ELSE




	       resultado=transform(importe,"999,999,999")

	     ENDIF
	      
	       IF MONEDA="EUR"
	              RESULTADO=RIGHT(RESULTADO,9)
	       ENDIF

	       @ prow()+1,4 say dtoc(fecha)
	       @ prow(),13 say left(matricula,10)
	       @ prow(),25 say left(origen1,10)+"-"+left(destino1,10)+LEFT(DESTINO2,10)
	       *resultado=wkilometros*wpreckilom
	       
	       @ prow(),79 say resultado+" "+moneda 
	       *if (origen2<>" ").or.(destino2<>" ")
	         @ prow()+1,25 say STR(kilometros,5)+" km."+STR(preckilom,10,2)+" eur/km."       
	       *endif
	       *if destino3<>" "
	       *    @ prow()+1,42 say destino3      
	       *endif
	       *if destino4<>" "
	           *?replicate(" ",43),destino4
	       *    @ prow()+1,42 say destino4     
	       *endif
	       *if destino5<>" "
	       *    @ prow()+1,42 say destino5 
	       *endif
	       if notas<>" "
	           @ prow()+1,25 say notas      
	       endif
	       indice=indice+1
	       wmoneda=moneda
	       if respuesta="i"
	                finaltotal=finaltotal+importe
	       endif
	       skip

	enddo
	?
	if respuesta<>"i"
	       finaltotal=finaltotal-iva
	endif
	if indice>1

		 IF WMONEDA="EUR"

		       resultadototal=transform(finaltotal,"999,999,999.99")
		    ELSE
		       resultadototal=transform(finaltotal,"999,999,999")
		 ENDIF
		      
		 IF WMONEDA="EUR"
		        RESULTADOTOTAL=RIGHT(RESULTADOTOTAL,9)

		 ENDIF
		     @ prow()+1,79 say "---------"
		     @ prow()+1,79 say resultadototal+" "+wmoneda
	endif
	if respuesta="i"
	       iva=0
	       iva=(finaltotal)/100*21
	endif
	IF WMONEDA="EUR"
	         resultadoiva=transform(iva,"999,999,999.99")
	     ELSE
	         resultadoiva=transform(iva,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOIVA=RIGHT(RESULTADOIVA,9)
	ENDIF

	@ prow()+2,65 say "21% IVA"
	@ prow(),79 say resultadoiva+" "+wmoneda
	@ prow()+1,79 say "---------"
	if respuesta="i"
	    finaltotal=finaltotal+iva
	endif
	if respuesta<>"i"
	       finaltotal=finaltotal+iva
	endif
	***************************
	******* tratamiento de euros
	*****************************
	euros=finaltotal/166.386
	reuros=transform(euros,"999,999,999.99")

	      




	************************************
	IF WMONEDA="EUR"

	        resultadofinal=transform(finaltotal,"999,999,999.99")
	     ELSE
	        resultadofinal=transform(finaltotal,"999,999,999")
	ENDIF
	       
	IF WMONEDA="EUR"
	       RESULTADOFINAL=RIGHT(RESULTADOFINAL,9)
	ENDIF

	@ prow()+1,67 say "TOTAL.."
	@ prow(),79 say resultadofinal+" "+wmoneda
	IF WMONEDA<>"EUR"
	
	   @ prow()+1,67 say reuros+"EUR"
	ENDIF
	*******************+
	@ prow()+1,79 say "========="
	*******impresion de notas

	    
	       @ prow()+3,25 say wnota1
	       @ prow()+1,25 say wnota2
	       @ prow()+1,25 say wnota3
	       @ prow()+1,25 say wnota4
	       @ prow()+1,25 say wnota5 
	       @ prow()+1,25 say wnota6
	       @ prow()+1,25 say wnota7
  	
  do case
   	case xx="I"
	   set print off
	   set printer to
	   set device to screen
    case xx="P"
       set device to screen
       modify file factura.txt noedit
        
       delete file factura.txt
  endcase

	SET CENTURY ON
ENDIF





ELSE && de if .not. prueba


IF WEMPRESA="AGENCIA CABRERO"
  DO AGENCIACABRERO
ELSE && DE AGENCIA CABRERO DENTRO DE  if .not. prueba



CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasnacional.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on

ENDIF && DE AGENCIA CABRERO

ENDIF && de if .not. prueba


WAIT WINDOW
RETURN
***********************************************************************
***********************************************************************
*********FIN DE PROCEDURE NACNUEV21PATINTER










**********************************
************FACTURA SIETE POR CIENTO
***********************************
PROCEDURE SIETEPORC
*********ENTRADA DE CONCEPTOS   

if respuesta<>"i"
wmodalidad="r"   

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
store space(10) to wreferencia
wpreckilom=0
wimporte=0
wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1
visor=.f.
do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       store space(15) to worigen2
       store space(15) to wdestino1

       store space(15) to wdestino2
       store space(15) to wdestino3
       store space(15) to wdestino4
       store space(15) to wdestino5



       store space(10) to wreferencia
       wpreckilom=0
       wimporte=0
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas

       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wmODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       *CLEAR GETS


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "Destino 5:        " get wdestino5
       @ 15,20 say "Nº Referencia     " get wreferencia
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte  PICTURE "999,999,999.99"
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Notas:            " get wnotas
       read
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -  "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       replace origen2    with worigen2
       replace destino1   with wdestino1

       replace destino2   with wdestino2
       replace destino3   with wdestino3
       replace destino4   with wdestino4
       replace destino5   with wdestino5
       replace referencia with wreferencia
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas
       visor=.t.
       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    a="s"
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    read
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo
if .not.visor
         clear
         @ 10,10 say "No se ha grabado ningun concepto por"
         @ 11,10 say "no haber introducido ninguna fecha."

else
clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*8
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with  wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


**IMPRESION DE NACIONAL

prueba=.F.

IF .not. prueba && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES


IF RESPUESTA="i"
       VISOR=.t.
ENDIF
if visor
XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file factura.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR

SET CENTURY OFF
set print font "courier new",11
*impresion de la direccion del cliente

@ 11,46 say wnombre
@ 12,46 say wdireccion
IF WAPDO<>" "
         @ 13,46 say wapdo 
ENDIF

if wpoligono<>" "
         @ 14,46 say wpoligono
endif
@ 15,46 say wcodpostal+" "+left(wpoblacion,25)
if wpais<>"ESPAÑA"
   @ 16,46 say wprovincia+" "+wpais
ELSE
   @ 16,46 say wprovincia
endif
@ 18,56 say "NIF "+wnif
if wpoligono= " "
endif

IF WAPDO=" "

ENDIF


SET CENTURY ON
@ 23,14 say wnumfac
@ 23,67 say fechafac
SET CENTURY OFF
@ 25,4 say "FORMA DE PAGO: "+wnformapago

set print font "courier new",10

********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
use concepto index infacfec,innumero,in1ref

seek wnumfac
if respuesta="i"
       finaltotal=0
endif
indice=0
@ 34,4 say ""
do  while numfac=wnumfac

     IF MONEDA="EUR"

       resultado=transform(importe,"999,999,999.99")
       ELSE




       resultado=transform(importe,"999,999,999")

     ENDIF
      
       IF MONEDA="EUR"
              RESULTADO=RIGHT(RESULTADO,9)
       ENDIF

       @ prow()+1,3 say dtoc(fecha)
       @ prow(),13 say left(matricula,10)
       @ prow(),25 say left(origen1,15)+"-"+left(destino1,15)
       @ prow(),78 say resultado+" "+moneda
       
       if (origen2<>" ").or.(destino2<>" ")
         @ prow()+1,25 say left(origen2,15)+"-"+destino2       
       endif
       if destino3<>" "
           @ prow+1,42 say destino3
       endif
       if destino4<>" "
           @ prow()+1,42 say destino4
       endif
       if destino5<>" "
           @ prow()+1,42 say destino5
       endif
       if notas<>" "
           @ prow()+1,25 say notas
       endif
       indice=indice+1
       wmoneda=moneda
       if respuesta="i"
                finaltotal=finaltotal+importe
       endif
       skip
enddo
?
if respuesta<>"i"
       finaltotal=finaltotal-iva
endif
if indice>1

 IF WMONEDA="EUR"

       resultadototal=transform(finaltotal,"999,999,999.99")
    ELSE
       resultadototal=transform(finaltotal,"999,999,999")
 ENDIF
       
 IF WMONEDA="EUR"
        RESULTADOTOTAL=RIGHT(RESULTADOTOTAL,9)

 ENDIF
     @ prow()+1,78 say "---------"
     @ prow()+1,78 say resultadototal+" "+wmoneda
endif
if respuesta="i"
       iva=0
       iva=(finaltotal)/100*8
endif
IF WMONEDA="EUR"
         resultadoiva=transform(iva,"999,999,999.99")
     ELSE
         resultadoiva=transform(iva,"999,999,999")
ENDIF
      
IF WMONEDA="EUR"
       RESULTADOIVA=RIGHT(RESULTADOIVA,9)
ENDIF

@ prow()+2,66 say " 8% IVA"
@ prow(),78 say resultadoiva+" "+wmoneda
@ prow()+1,78 say "---------"
if respuesta="i"
    finaltotal=finaltotal+iva
endif
if respuesta<>"i"
       finaltotal=finaltotal+iva
endif
***************************
******* tratamiento de euros
*****************************
euros=finaltotal/166.386
reuros=transform(euros,"999,999,999.99")

      




************************************
IF WMONEDA="EUR"

        resultadofinal=transform(finaltotal,"999,999,999.99")
     ELSE
        resultadofinal=transform(finaltotal,"999,999,999")
ENDIF
       
IF WMONEDA="EUR"
       RESULTADOFINAL=RIGHT(RESULTADOFINAL,9)
ENDIF

@ prow()+1,66 say "TOTAL.."
@ prow(),78 say resultadofinal+" "+wmoneda
IF WMONEDA<>"EUR"

   @ prow()+1,66 say reuros+"EUR"
ENDIF
********************

@ prow()+1,78 say "========="
*******impresion de notas

      
       @ prow()+3,25 say wnota1
       
       @ prow()+1,25 say wnota2
       
       @ prow()+1,25 say wnota3
       
       @ prow()+1,25 say wnota4
       
       @ prow()+1,25 say wnota5

       @ prow()+1,25 say wnota6
       @ prow()+1,25 say wnota7
  do case
   	case xx="I"
	   set print off
	   set printer to
	   set device to screen
    case xx="P"
       set device to screen
       modify file factura.txt noedit
        
       delete file factura.txt
  endcase
SET CENTURY ON
endif


ELSE && de if .not. prueba

CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasnacional7.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on



ENDIF && de if .not. prueba


WAIT WINDOW
RETURN

**************************************************************************
**************************************************************************
**************************************************************************
*****FIN PROCEDURE SIETEPORC
**************************************************************************




**********************************
************FACTURA DIEZ POR CIENTO
***********************************
PROCEDURE DIEZPORC
*********ENTRADA DE CONCEPTOS   

if respuesta<>"i"
wmodalidad="10"   

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
store space(10) to wreferencia
wpreckilom=0
wimporte=0
wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1
visor=.f.
do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       store space(15) to worigen2
       store space(15) to wdestino1

       store space(15) to wdestino2
       store space(15) to wdestino3
       store space(15) to wdestino4
       store space(15) to wdestino5



       store space(10) to wreferencia
       wpreckilom=0
       wimporte=0
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas

       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wmODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       *CLEAR GETS


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "Destino 5:        " get wdestino5
       @ 15,20 say "Nº Referencia     " get wreferencia
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte  PICTURE "999,999,999.99"
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Notas:            " get wnotas
       read
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -  "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       replace origen2    with worigen2
       replace destino1   with wdestino1

       replace destino2   with wdestino2
       replace destino3   with wdestino3
       replace destino4   with wdestino4
       replace destino5   with wdestino5
       replace referencia with wreferencia
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas
       visor=.t.
       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    a="s"
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    read
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo
if .not.visor
         clear
         @ 10,10 say "No se ha grabado ningun concepto por"
         @ 11,10 say "no haber introducido ninguna fecha."

else
clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*10
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with  wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


**IMPRESION DE NACIONAL

prueba=.F.

IF .not. prueba && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES


IF RESPUESTA="i"
       VISOR=.t.
ENDIF
if visor
XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file factura.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR

SET CENTURY OFF
set print font "courier new",11
*impresion de la direccion del cliente

@ 11,46 say wnombre
@ 12,46 say wdireccion
IF WAPDO<>" "
         @ 13,46 say wapdo 
ENDIF

if wpoligono<>" "
         @ 14,46 say wpoligono
endif
@ 15,46 say wcodpostal+" "+left(wpoblacion,25)
if wpais<>"ESPAÑA"
   @ 16,46 say wprovincia+" "+wpais
ELSE
   @ 16,46 say wprovincia
endif
@ 18,56 say "NIF "+wnif
if wpoligono= " "
endif

IF WAPDO=" "

ENDIF


SET CENTURY ON
@ 23,14 say wnumfac
@ 23,67 say fechafac
SET CENTURY OFF
@ 25,4 say "FORMA DE PAGO: "+wnformapago

set print font "courier new",10

********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
use concepto index infacfec,innumero,in1ref

seek wnumfac
if respuesta="i"
       finaltotal=0
endif
indice=0
@ 34,4 say ""
do  while numfac=wnumfac

     IF MONEDA="EUR"

       resultado=transform(importe,"999,999,999.99")
       ELSE




       resultado=transform(importe,"999,999,999")

     ENDIF
      
       IF MONEDA="EUR"
              RESULTADO=RIGHT(RESULTADO,9)
       ENDIF

       @ prow()+1,3 say dtoc(fecha)
       @ prow(),13 say left(matricula,10)
       @ prow(),25 say left(origen1,15)+"-"+left(destino1,15)
       @ prow(),78 say resultado+" "+moneda
       
       if (origen2<>" ").or.(destino2<>" ")
         @ prow()+1,25 say left(origen2,15)+"-"+destino2       
       endif
       if destino3<>" "
           @ prow+1,42 say destino3
       endif
       if destino4<>" "
           @ prow()+1,42 say destino4
       endif
       if destino5<>" "
           @ prow()+1,42 say destino5
       endif
       if notas<>" "
           @ prow()+1,25 say notas
       endif
       indice=indice+1
       wmoneda=moneda
       if respuesta="i"
                finaltotal=finaltotal+importe
       endif
       skip
enddo
?
if respuesta<>"i"
       finaltotal=finaltotal-iva
endif
if indice>1

 IF WMONEDA="EUR"

       resultadototal=transform(finaltotal,"999,999,999.99")
    ELSE
       resultadototal=transform(finaltotal,"999,999,999")
 ENDIF
       
 IF WMONEDA="EUR"
        RESULTADOTOTAL=RIGHT(RESULTADOTOTAL,9)

 ENDIF
     @ prow()+1,78 say "---------"
     @ prow()+1,78 say resultadototal+" "+wmoneda
endif
if respuesta="i"
       iva=0
       iva=(finaltotal)/100*10
endif
IF WMONEDA="EUR"
         resultadoiva=transform(iva,"999,999,999.99")
     ELSE
         resultadoiva=transform(iva,"999,999,999")
ENDIF
      
IF WMONEDA="EUR"
       RESULTADOIVA=RIGHT(RESULTADOIVA,9)
ENDIF

@ prow()+2,66 say "10% IVA"
@ prow(),78 say resultadoiva+" "+wmoneda
@ prow()+1,78 say "---------"
if respuesta="i"
    finaltotal=finaltotal+iva
endif
if respuesta<>"i"
       finaltotal=finaltotal+iva
endif
***************************
******* tratamiento de euros
*****************************
euros=finaltotal/166.386
reuros=transform(euros,"999,999,999.99")

      




************************************
IF WMONEDA="EUR"

        resultadofinal=transform(finaltotal,"999,999,999.99")
     ELSE
        resultadofinal=transform(finaltotal,"999,999,999")
ENDIF
       
IF WMONEDA="EUR"
       RESULTADOFINAL=RIGHT(RESULTADOFINAL,9)
ENDIF

@ prow()+1,66 say "TOTAL.."
@ prow(),78 say resultadofinal+" "+wmoneda
IF WMONEDA<>"EUR"

   @ prow()+1,66 say reuros+"EUR"
ENDIF
********************

@ prow()+1,78 say "========="
*******impresion de notas

      
       @ prow()+3,25 say wnota1
       
       @ prow()+1,25 say wnota2
       
       @ prow()+1,25 say wnota3
       
       @ prow()+1,25 say wnota4
       
       @ prow()+1,25 say wnota5

       @ prow()+1,25 say wnota6
       @ prow()+1,25 say wnota7
  do case
   	case xx="I"
	   set print off
	   set printer to
	   set device to screen
    case xx="P"
       set device to screen
       modify file factura.txt noedit
        
       delete file factura.txt
  endcase
SET CENTURY ON
endif


ELSE && de if .not. prueba

CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasnacional7.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on



ENDIF && de if .not. prueba


WAIT WINDOW
RETURN

**************************************************************************
**************************************************************************
**************************************************************************
*****FIN PROCEDURE DIEZPORC
**************************************************************************





**********************************
************FACTURA ARCESE
***********************************
PROCEDURE ARCESE
*********ENTRADA DE CONCEPTOS

if respuesta<>"i"
*********ES DECIR IMPRESION DE LA FACTURA POSTERIOR A LA INTRODUCCION
*********DE DATOS SERIA RESPUESTA=i
modalidad="a"
wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
wkilometros=0
wpreckilom=0
wimporte=0
*store space(3) to wmoneda
WMONEDA="LIT"
wtransport="CABRERO"
store space(10) to wconductor
store space(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC


GO BOTTOM
orden=0


       wnumero=numero+1
       orden=orden+1


do while dac<>0


       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre+" Modalidad:"+wmodalidad
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen1
store space(15) to wdestino1
wkilometros=0
wpreckilom=0
wimporte=0
store space(3) to wmoneda

WTRANSPORT="CABRERO"
store space(10) to wconductor
STORE SPACE(10) to wmcabeza


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen :          " get worigen1
       @ 9,20  say "Destino :         " get wdestino1
       @ 10,20 say "Kilometros :      " get wkilometros
       @ 11,20 say "Precio/kilometro: " get wpreckilom  picture "9.99"
       @ 12,20 say "Importe:          " get wimporte
       @ 13,20 say "Moneda:           " get wmoneda
       @ 15,20 say "Transportista:    " get wtransport
       @ 16,20 say "Conductor:        " get wconductor
       @ 17,20 say "Matricula de la cabeza: " get wmcabeza
       read
       wimporte=wkilometros*wpreckilom
       
        wtotal=wtotal+wimporte


     IF  DTOC(WFECHA)<>"  -  -  "


       append blank
       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1     with worigen1
       replace destino1   with wdestino1
       replace kilometros with wkilometros
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza

       wnumero=numero+1
       orden=orden+1
    ENDIF
    A="s"
    @ 20,35 say "(s)seguir /(t) terminado. " GET A
    READ
    dac=asc(a)
do case
    case (dac=116).or.(dac=84)
       dac=0
endcase
enddo




**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac

finaltotal=wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago
replace modalidad with wmodalidad

endif
***de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


***IMPRESION


SET PRINT ON prompt
SET CENTURY OFF
*impresion de la direccion del cliente
?
?
?
?
?
?
?
?
?
?
?"          ",wnombre
?"          ",wdireccion
?"          ",wcodpostal,left(wpoblacion,25)
if wpais<>"ESPAÑA"
   ?"          ",wprovincia+" "+wpais
else
   ?"          ",wprovincia
endif
?
?replicate(" ",45),"NIF ",wnif
?
?
?
?


?
?
SET CENTURY ON
?replicate(" ",11),wnumfac,replicate(" ",43),fechafac
SET CENTURY OFF
?
?"   FORMA DE PAGO: ",wnformapago
?
?chr(27)+chr(77)
?
?
?
?REPLICATE(" ",23),"TRANSPORTE EFECTUADO CON SEMIRREMOLQUE"
?REPLICATE(" ",23),"DE SU PROPIEDAD"
?
?
********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
use concepto index infacfec,innumero,inclife

seek wnumfac
if respuesta="i"
       finaltotal=0
endif
do  while numfac=wnumfac
       resultado=transform(importe,"999,999,999.99")

      
      


       ?"  ",dtoc(fecha)+"  "+left(matricula,8)+"   "+left(origen1,15),"-",left(destino1,14),kilometros,"Km",preckilom,"  ",resultado,moneda
       METAL=MONEDA
       if respuesta="i"

                finaltotal=finaltotal+importe
       endif

***************************
******* tratamiento de euros
*****************************
euros=finaltotal/1936.27
reuros=transform(euros,"999,999,999.99")

       




************************************
       skip
enddo
?

?replicate(" ",76),"----------"
resultadofinal=transform(finaltotal,"999,999,999.99")

      
?replicate(" ",67),"TOTAL..",resultadofinal,METAL
IF METAL<>"EUR"
*******tratamiento de euros
?CHR(27)+CHR(52)
?replicate(" ",67),"       ",reuros,"EUR"
*******************+
ENDIF
?CHR(27)+CHR(53)
?
?
?
?chr(27)+chr(80)
set print off
set printer to
SET CENTURY ON
WAIT WINDOW
RETURN



**********************************
************FACTURA INTERNACIONAL  ( sin iva )
***********************************
PROCEDURE DEFUERA
*********ENTRADA DE CONCEPTOS
if respuesta<>"i"

wmodalidad="e"
wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
store space(10) to wreferencia
wpreckilom=0
wimporte=0
wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
store space(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0

       wnumero=numero+1
       orden=orden+1
do while dac<>0


       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wMODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS


wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen1

store space(15) to worigen2
store space(15) to wdestino1

store space(15) to wdestino2
store space(15) to wdestino3
store space(15) to wdestino4
store space(15) to wdestino5
store space(10) to wreferencia
wpreckilom=0
wimporte=0
Wmoneda="EUR"
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
store space(38) to wnotaS

       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "Destino 5:        " get wdestino5
       @ 15,20 say "Nº Referencia:      " get wreferencia
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte PICTURE "999,999,999.99"
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula de la cabeza: " get wmcabeza
       @ 23,20 say "Nota              " get wnotaS
       read
       wtotal=wtotal+wimporte


     IF  DTOC(WFECHA)<>"  -  -  "


       append blank
       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       replace origen2    with worigen2
       replace destino1   with wdestino1
       replace destino2   with wdestino2
       replace destino3   with wdestino3
       replace destino4   with wdestino4
       replace destino5   with wdestino5
       replace referencia with wreferencia
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notaS      with wnotaS

       wnumero=numero+1
       orden=orden+1
     endif
    a="s"
    @ 24,35 say "(s)seguir /(t) terminado. "  get a
    read
    dac=asc(a)
do case
    case (dac=116).or.(dac=84)
       dac=0
endcase
enddo

clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac

FINALTOTAL=WTOTAL

replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
replace moneda with wmoneda
replace formapago with wnformapago
replace nota1 with wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
***de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


***IMPRESION

prueba=.F.

IF .not. prueba && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES


XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file factura.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR

SET CENTURY OFF
*set print font "courier new",11
SET PRINTER FONT "monospac821 bt",11
*impresion de la direccion del cliente

@ 10,46 say wnombre
*?"          ",wdireccion
@ 11,46 say wdireccion

IF WAPDO<>" "
         @ 12,46 say wapdo
ENDIF

if wpoligono<>" "
         @ 13,46 say wpoligono
endif
@ 14,46 say wcodpostal+" "+left(wpoblacion,25)
if wpais<>"ESPAÑA"
   @ 15,46 say wprovincia+" "+wpais
else
   @ 15,46 say wprovincia
endif

@ 17,56 say "NIF "+wnif
if wpoligono= " "
       * ?
endif


SET CENTURY ON
@ 20,14 say wnumfac
@ 20,67 say fechafac
SET CENTURY OFF

@ 23,4 say "FORMA DE PAGO: "+wnformapago

*set print font "courier new",10
SET PRINTER FONT "monospac821 bt",10
********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
use concepto index infacfec,innumero,in1ref

seek wnumfac
if respuesta="i"
       finaltotal=0
endif
@ 29,5 say ""
do  while numfac=wnumfac
     IF MONEDA="EUR"
       *******para que siga sirviendo para las facturas en pesetas
       resultado=transform(importe,"999,999,999.99")
      ELSE

       resultado=transform(importe,"999,999,999")
     ENDIF
      
       IF MONEDA="EUR"
            RESULTADO=RIGHT(RESULTADO,9)
       ENDIF
       IF KILOMETROS<>0
       
      
  
      
       @ prow()+1,4 say dtoc(fecha)
       @ prow(),13 say left(matricula,10)
       @ prow(),25 say left(origen1,15)+"-"+left(destino1,15)
       @ prow(),58 say str(kilometros,5)+" km." 
       @ prow(),79 say resultado+" "+moneda
       
       ELSE

       @ prow()+1,3 say dtoc(fecha)
       @ prow(),13 say left(matricula,10)
       @ prow(),25 say left(origen1,15)+"-"+left(destino1,15)
       @ prow(),79 say resultado+" "+moneda
       




       ENDIF

       if (origen2<>" ").or.(destino2<>" ")
         @ prow()+1,25 say left(origen2,15)+"-"+destino2 
       endif
       if destino3<>" "
           @ prow()+1,42 say destino3       
       endif
       if destino4<>" "
           @ prow()+1,42 say destino4     
       endif
       if destino5<>" "
           @ prow()+1,42 say destino5
       endif
       if notas<>" "
           @ prow()+1,25 say notas
       endif
       WMONEDA=MONEDA
       if respuesta="i"
                finaltotal=finaltotal+importe
       endif

***************************
******* tratamiento de euros
*****************************
DO CASE
     CASE  MONEDA="LIT"
          euros=finaltotal/1936.27
     CASE MONEDA="PTS"
          euros=finaltotal/166.386
     case moneda="FF"
          euros=finaltotal/6.55957
     otherwise
          euros=0
endcase
reuros=transform(euros,"999,999,999.99")

       




************************************
   * endif
       skip
enddo

@ prow()+1,79 say "-------------"
  if wmoneda="EUR"
    resultadofinal=transform(finaltotal,"999,999,999.99")
   ELSE

    resultadofinal=transform(finaltotal,"999,999,999")
  ENDIF
       
     IF WMONEDA="EUR"
       RESULTADOFINAL=RIGHT(RESULTADOFINAL,9)
     ENDIF
@ prow()+1,67 say "TOTAL.."
@ prow(),79 say resultadofinal+" "+wmoneda 

*******tratamiento de euros
IF EUROS<>0
  
  @ prow()+2,79 say reuros+" EUR"
  *******************
  
  @ prow()+3,79 say "============="
ENDIF


*******impresion de notas
      
       @ prow()+3,25 say wnota1
       @ prow()+1,25 say wnota2
       @ prow()+1,25 say wnota3
       @ prow()+1,25 say wnota4
       @ prow()+1,25 say wnota5

       @ prow()+1,25 say wnota6
       @ prow()+1,25 say wnota7
   
  do case
   	case xx="I"
	   set print off
	   set printer to
	   set device to screen
    case xx="P"
       set device to screen
       modify file factura.txt noedit
        
       delete file factura.txt
  endcase

SET CENTURY ON

ELSE && de if .not. prueba

CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasextranjero.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on



ENDIF && de if .not. prueba



WAIT WINDOW
RETURN
************************************************
*******PROCEDURE MODIFICACIONES
**************************************************
procedure modif
parameters wnumfac,WMODALIDAD
private wresultado,iresultado
clear

use concepto index infacfec,innumero,in1ref

seek wnumfac
if found()
orden=0
       do while wnumfac=numfac
       orden=orden+1
           clear

      @ 1,10  say "Nombre: " +wclave +"      M O D I F I C A R"
      @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)

      @ 4,10  say "Referencia: "+str(numero,7) +"  Numero de orden: "+str(orden,4)


       @ 6,20  say "Fecha :           " get fecha
       @ 7,20  say "Matricula :       " get matricula
       @ 8,20  say "Origen 1:         " get origen1
       @ 9,20  say "Origen 2:         " get origen2
       @ 10,20 say "Destino 1:        " get destino1
       @ 11,20 say "Destino 2:        " get destino2
       @ 12,20 say "Destino 3:        " get destino3
       @ 13,20 say "Destino 4:        " get destino4
       @ 14,20 say "Destino 5:        " get destino5
       @ 15,20 say "Nº Referencia     " get referencia
       @ 15,60 SAY "CMR " GET CMR
       @ 16,20 say "Precio/kilometro: " get preckilom
       @ 17,20 say "Importe:          " get importe
       @ 18,20 say "Moneda:           " get moneda
       @ 20,20 say "Transportista:    " get transport
       @ 21,20 say "Conductor:        " get conductor
       @ 22,20 say "Matricula de la cabeza: " get mcabeza
       @ 23,20 say "Nota              " get notaS
       @ 24,20 say "Nota 2            " get notas2
       read
       IF WMODALIDAD="a"
           WIMPORTE=PRECKILOM*KILOMETROS
           REPLACE IMPORTE WITH WIMPORTE
       ENDIF
       skip


       enddo


endif


wresultado=0
iresultado=0
i7resultado=0
i18resultado=0
i10resultado=0
i21resultado=0
seek wnumfac
if found()
      do while wnumfac=numfac
              wresultado =wresultado+importe
              skip
      enddo
endif
iresultado =wresultado+wresultado/100*16

i7resultado =wresultado+wresultado/100*8  && 8%
i18resultado =wresultado+wresultado/100*18
i10resultado=wresultado+wresultado/100*10
i21resultado=wresultado+wresultado/100*21
use facturas index infac,inclife
seek wnumfac
IF MODALIDAD<>" "
DO CASE
     CASE modalidad="n".or.modalidad="o"
       REPLACE importe WITH iresultado
     CASE MODALIDAD="r" && 8%
       REPLACE IMPORTE WITH I7RESULTADO
     CASE MODALIDAD="8" && 18%
       REPLACE IMPORTE WITH I18RESULTADO
     CASE modalidad="10" && 10%
       replace importe WITH i10resultado
     CASE modalidad="21" && 21%
       replace importe WITH i21resultado
     CASE MODALIDAD="m" && SMET
       replace importe WITH i21resultado
     OTHERWISE
       REPLACE importe WITH wresultado
ENDCASE
ENDIF
clear
WFECHA=FECHA

@ 4,10 say "Cliente: " +cliente

@ 5,10 say "Factura:  " +factura
@ 6,10 say "Fecha:    " get Wfecha
@ 7,10 say "Fecha env." get envfecha
@ 8,10 say "Pagado:   " get pagado
@ 9,10 say "Importe:  " get importe
@ 10,10 say "Moneda:   " get moneda
@ 11,10 say "Forma pago: " get formapago
@ 12,10 say "Nota 1: " get nota1
@ 13,10 say "Nota 2: " get nota2
@ 14,10 say "Nota 3: " get nota3

@ 15,10 say "Nota 4: " get nota4
@ 16,10 say "Nota 5: " get nota5
@ 17,10 say "Nota 6: " get nota6

@ 18,10 say "Nota 7: " get nota7
@ 19,10 say "Modalidad: " +modalidad
read
REPLACE FECHA WITH WFECHA
USE
return



************************************************
*******PROCEDURE consulta de facturas
**************************************************
procedure faconsul
parameters wnumfac,WMODALIDAD
private wresultado,iresultado
clear

use concepto index infacfec,innumero,in1ref

seek wnumfac
if found()
orden=0
       do while wnumfac=numfac
       orden=orden+1
           clear

      @ 1,10  say "Nombre: " +wclave +"      C O N S U L T A"
      @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)

      @ 4,10  say "Referencia: "+str(numero,7) +"  Numero de orden: "+str(orden,4)


       @ 6,20  say "Fecha :           " +DTOC(fecha)
       @ 7,20  say "Matricula :       " +matricula
       @ 8,20  say "Origen 1:         " +origen1
       @ 9,20  say "Origen 2:         " +origen2
       @ 10,20 say "Destino 1:        " +destino1
       @ 11,20 say "Destino 2:        " +destino2
       @ 12,20 say "Destino 3:        " +destino3
       @ 13,20 say "Destino 4:        " +destino4
       @ 14,20 say "Destino 5:        " +destino5
       @ 15,20 say "Nº Referencia     " +referencia
       @ 15,60 SAY "CMR " +CMR
       @ 16,20 say "Precio/kilometro: " +str(preckilom,10,2)
       @ 17,20 say "Importe:          " +str(importe,12,2)
       @ 18,20 say "Moneda:           " +moneda
       @ 20,20 say "Transportista:    " +transport
       @ 21,20 say "Conductor:        " +conductor
       @ 22,20 say "Matricula de la cabeza: " +mcabeza
       @ 23,20 say "Nota              " +notaS
       read
       IF WMODALIDAD="a"
           WIMPORTE=PRECKILOM*KILOMETROS
           REPLACE IMPORTE WITH WIMPORTE
       ENDIF
       skip


       enddo


endif


wresultado=0
iresultado=0
seek wnumfac
if found()
      do while wnumfac=numfac
              wresultado =wresultado+importe
              skip
      enddo
endif
iresultado =wresultado+wresultado/100*16
i18resultado =wresultado+wresultado/100*18
i7resultado =wresultado+wresultado/100*8
i10resultado=wresultado+wresultado/100*10
i21resultado=wresultado+wresultado/100*21
use facturas index infac,inclife
seek wnumfac
IF MODALIDAD<>" "
DO CASE
     CASE modalidad="n".or.modalidad="o"
       REPLACE importe WITH iresultado
     CASE MODALIDAD="7"
       REPLACE IMPORTE WITH I7RESULTADO
     
     CASE MODALIDAD="8"
       REPLACE IMPORTE WITH I18RESULTADO
     CASE modalidad="10"
       replace importe WITH i10resultado
     CASE modalidad="21".OR.MODALIDAD="m"
       replace importe WITH i21resultado
     
     OTHERWISE
       REPLACE importe WITH wresultado
ENDCASE
ENDIF
clear

@ 4,10 say "Cliente: " +cliente

@ 5,10 say "Factura:  " +factura
@ 6,10 say "Fecha:    " +DTOC(fecha)
@ 7,10 say "Fecha env." +DTOC(envfecha)
p="no"
do case
   case pagado
       p="si"
   case .not.pagado
       p="no"
endcase
@ 8,10 say "Pagado:   " +p
@ 9,10 say "Importe:  " +str(importe,12,2)
@ 10,10 say "Moneda:   " +moneda
@ 11,10 say "Forma pago: " +formapago
@ 12,10 say "Nota 1: " +nota1
@ 13,10 say "Nota 2: " +nota2
@ 14,10 say "Nota 3: " +nota3

@ 15,10 say "Nota 4: " +nota4
@ 16,10 say "Nota 5: " +nota5
@ 17,10 say "Nota 6: " +nota6

@ 18,10 say "Nota 7: " +nota7
@ 19,10 say "Modalidad: " +modalidad
read
return


**********************************
************FACTURA Normal
***********************************
PROCEDURE FACTURANormal

*********ENTRADA DE CONCEPTOS

if respuesta<>"i"

wmodalidad="o"
wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
wkilometros=0
wpreckilom=0
wimporte=0
store space(3) to wmoneda
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1

do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       store space(15) to worigen2
       store space(15) to wdestino1

       store space(15) to wdestino2
       store space(15) to wdestino3
       store space(15) to wdestino4
       store space(15) to wdestino5



       wkilometros=0
       wpreckilom=0
       wimporte=0
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas

       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wMODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       *CLEAR GETS
       

       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       
       @ 15,20 say "Kilometros :      " get wkilometros
       @ 16,20 say "Precio/kilometro: " get wpreckilom
       @ 17,20 say "Importe:          " get wimporte
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Concepto:         " get wnotas
       read
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -  "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
      
       replace kilometros with wkilometros
       replace preckilom  with wpreckilom
       replace importe    with wimporte
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas

       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    a="s"
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    read
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo

clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*16
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with  wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


***IMPRESION DE NACIONAL

prueba=.F.

IF .not. prueba && PRUEBA DE IMPRESION CON GENERADOR DE INFORMES


XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file factura.txt
	    
	   
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF

SET PRINTER FONT "COURIER NEW",11
SET CENTURY OFF

*impresion de la direccion del cliente

@ 11,8 say wnombre
@ 12,8 say wdireccion
IF WAPDO<>" "
	         @ 13,8 say wapdo 
ENDIF
if wpoligono<>" "
         @ 14,8 say wpoligono 
endif
@ 15,8 say wcodpostal+" "+left(wpoblacion,25)
if wpais<>"ESPAÑA"
   @ 16,8 say wprovincia+" "+wpais
else
   *?
   @ 16,8 say wprovincia
endif

@ 18,56 say "NIF: "+wnif
if wpoligono= " "
        *?
endif

SET CENTURY ON
@ 23,14 say wnumfac
@ 23,67 say fechafac
SET CENTURY OFF

@ 25,4 say "FORMA DE PAGO: "+WNFORMAPAGO

SET PRINTER FONT "COURIER NEW",10
@ 34,4 SAY " "
********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
use concepto index infacfec,innumero

seek wnumfac
if respuesta="i"
       finaltotal=0
endif
indice=0
do  while numfac=wnumfac
       resultado=transform(importe,"999,999,999.99")
       IF MONEDA="EUR"
               RESULTADO=RIGHT(RESULTADO,9)
       ENDIF
      
       @ PROW()+1,3 SAY DTOC(FECHA)
       @ PROW(),13 SAY LEFT(MATRICULA,10)
       @ PROW(),25 SAY NOTAS
       @ PROW(),78 SAY RESULTADO                                                         
       if (origen2<>" ").or.(destino2<>" ")
           ?replicate(" ",25),left(origen2,15),"-",destino2
               
       endif
       if destino3<>" "
           ?replicate(" ",43),destino3
       endif
       if destino4<>" "
           ?replicate(" ",43),destino4
       endif
       if destino5<>" "
           ?replicate(" ",43),destino5
       endif
       
       WMONEDA=MONEDA
       indice=indice+1
        
       if respuesta="i"
                finaltotal=finaltotal+importe
       endif
   
       skip
enddo

if respuesta<>"i"
       finaltotal=finaltotal-iva
endif
if indice>1
     resultadototal=transform(finaltotal,"999,999,999.99")
     IF WMONEDA="EUR"
           RESULTADOTOTAL=RIGHT(RESULTADOTOTAL,9)
     ENDIF
      
     @ PROW()+1,78 SAY "---------"
     @ PROW()+1,78 SAY RESULTADOTOTAL
endif
if respuesta="i"
       iva=0
       iva=(finaltotal)/100*16
endif
resultadoiva=transform(iva,"999,999,999.99")
IF WMONEDA="EUR"
      RESULTADOIVA=RIGHT(RESULTADOIVA,9)
ENDIF
      
@ PROW()+1,66 SAY "16% IVA"
@ PROW(),78 SAY RESULTADOIVA
@ PROW(),78 SAY "---------"
if respuesta="i"
    finaltotal=finaltotal+iva
endif
if respuesta<>"i"
       finaltotal=finaltotal+iva
endif
resultadofinal=transform(finaltotal,"999,999,999.99")
IF WMONEDA="EUR"
      RESULTADOFINAL=RIGHT(RESULTADOFINAL,9)
ENDIF
       
@ PROW()+1,66 SAY "TOTAL.. "
@ PROW(),78 SAY RESULTADOFINAL

@ PROW()+1,78 SAY "========="
*******impresion de notas

      
       @ PROW()+2,25 SAY WNOTA1 
       @ PROW()+1,25 SAY WNOTA2
       @ PROW()+1,25 SAY WNOTA3
       @ PROW()+1,25 SAY WNOTA4
       @ PROW()+1,25 SAY WNOTA5
       @ PROW()+1,25 SAY WNOTA6
       @ PROW()+1,25 SAY WNOTA7
  do case
   	case xx="I"
	   set print off
	   set printer to
	   set device to screen
    case xx="P"
       set device to screen
       modify file factura.txt noedit
        
       delete file factura.txt
  endcase

SET CENTURY ON



ELSE && de if .not. prueba

CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\informefacturasnacionalnormal.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on



ENDIF && de if .not. prueba


WAIT WINDOW
RETURN
************************************************
*******PROCEDURE LISTADOS  POR CLIENTES
************************************************

PROCEDURE LISTCLI
PARAMETERS MES,ano

       use facturas index inCLIFE,infac
       XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file archivo.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
       
       SET PRINT FONT "COURIER NEW",10
       SET CENTURY OFF
       @ 1,1 SAY "   Facturas no pagadas"
       @ 2,1 SAY "   ==================="
       
       go top
       wfimporte=0
       wlimporte=0
       wpimporte=0
       WEIMPORTE=0
       do while .not. eof()
               if .not.(pagado).and.(year(fecha)=ano).and.(month(fecha)=MES)
                   @ PROW()+1,1 SAY CLIENTE
                   @ PROW(),12 SAY FACTURA
                   @ PROW(),24 SAY FECHA
                   @ PROW(),36 SAY STR(IMPORTE,10,2)
                   @ PROW(),48 SAY MONEDA
                   @ PROW(),60 SAY FORMAPAGO
                   
                   do case
                     case (moneda="PTS").or.(moneda="pts").or.(moneda=" ")
                        wpimporte=importe+wpimporte
                     case (moneda="FF")
                        wfimporte=importe+wfimporte
                     case moneda="LIT"
                        wlimporte=importe+wlimporte
                     CASE MONEDA="EUR"
                        WEIMPORTE=IMPORTE+WEIMPORTE
                   endcase
               endif
               skip
               IF PROW()+1>71
                    if xx="I" 
                     EJECT
                    endif 
               ENDIF   
       
       enddo
       IF PROW()+1>66
            if xx="I"
             EJECT
            endif
        ENDIF     
             
       
       
       @ PROW()+2,1 SAY "Pesetas:  "+" "+STR(wpimporte,14,2)
       @ PROW()+1,1 SAY "Liras:    "+" "+STR(wlimporte,14,2)
       @ PROW()+1,1 SAY "Francos:  "+" "+STR(wfimporte,14,2)
       @ PROW()+1,1 SAY "Euros:    "+" "+STR(weimporte,14,2)
       resultado=wpimporte+wlimporte/100*8.59+wfimporte*25.25+weimporte*166.386
       @ PROW()+1,1 SAY "Total:    "+" "+STR(resultado,14,2)
	  do case
	   	case xx="I"
		   set print off
		   set printer to
		   set device to screen
	    case xx="P"
	       set device to screen
	       modify file archivo.txt noedit
	        
	       delete file archivo.txt
	  endcase
       
       SET CENTURY ON
       WAIT WINDOW
RETURN

******************************************
*********PROCEDIMIENTO LISTADO POR NUMERO DE FACTURA
*****************************************



PROCEDURE LISTFAC
PARAMETERS MES,ano
     SELECT 1
       use facturas index inFAC,inclife
     SELECT 2  
       USE clientes INDEX inclave
     SELECT 1  
       XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file archivo.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
     
       SET PRINT FONT "COURIER NEW",10
       SET CENTURY OFF
       @ 1,1 SAY "   Facturas no pagadas"
       @ 2,1 SAY "   ==================="
      
       go top
       WNIF=""
       WCLIENTE=""
       wfimporte=0
       wIimporte=0
       wpimporte=0
       WEIMPORTE=0
       do while .not. eof()
               if .not.(pagado).and.(year(fecha)=ano).and.(month(fecha)=MES)
                   
                   @ PROW()+1,1 SAY CLIENTE
                   @ PROW(),12 SAY FACTURA
                   @ PROW(),24 SAY FECHA
                   @ PROW(),36 SAY STR(IMPORTE,10,2)
                   @ PROW(),48 SAY MONEDA
                   @ PROW(),60 SAY FORMAPAGO
                   WCLIENTE=CLIENTE
                   SELECT 2
                   SEEK WCLIENTE
                   WNIF=NIF
                   
                   SELECT 1
                   do case
                     case (SUBSTR(WNIF,1,2)="DE")
                        wEimporte=importe+wEimporte
                     case (SUBSTR(WNIF,1,2)="FR")
                        wfimporte=importe+wfimporte
                     case (SUBSTR(WNIF,1,2)="IT")
                        wIimporte=importe+wIimporte
                     case (SUBSTR(WNIF,1,2)="PT")
                        WPIMPORTE=IMPORTE+WPIMPORTE
                     case (SUBSTR(WNIF,1,2)="ES")
                        WPIMPORTE=IMPORTE+WEIMPORTE
                     OTHERWISE
                        wEimporte=importe+wEimporte
                   endcase
                                   
               endif
               skip
               IF PROW()+1>71
                   if xx="I"  
                     EJECT
                   endif
               ENDIF   
       
       
       enddo
       IF PROW()+1>66
           if xx="I"
             EJECT
           endif
       ENDIF     
      
     
       @ PROW()+2,1 SAY "ESPAÑA:  "+" "+STR(wEimporte,14,2)
       @ PROW()+1,1 SAY "ITALIA:    "+" "+STR(wIimporte,14,2)
       @ PROW()+1,1 SAY "FRANCIA:  "+" "+STR(wfimporte,14,2)
       *?"Euros:    ",STR(weimporte,14,2),"son ",STR(weimporte*166.386,14,2),"pts"
       @ PROW()+1,1 SAY "PORTUGAL:    "+" "+STR(wPimporte,14,2)
       resultado=wpimporte+wIimporte+wfimporte+weimporte
       
       *?"Total:    ",STR(resultado,14,2),"pts"
       @ PROW()+1,1 SAY "Total:    "+" "+STR(resultado,14,2)
		  do case
		   	case xx="I"
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       modify file archivo.txt noedit
		        
		       delete file archivo.txt
		  endcase
       
       SET CENTURY ON
       WAIT WINDOW
RETURN


******************************************
*********PROCEDIMIENTO LISTADO POR NUMERO DE FACTURA  de todo lo facturado
*****************************************             ----------------------
**************************************************************************


PROCEDURE TLISTFAC
PARAMETERS MES,ano

       use facturas index inFAC,inclife
       XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file archivo.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	    SET PRINT FONT "COURIER NEW",10
	
	ENDIF
	CLEAR
       
       
       
       SET CENTURY OFF
       wfecha=ctod("01-"+str(mes,2)+"-"+str(ano,4))
       @ 1,1 SAY "    Facturas de "+cmonth(wfecha)+" de "+str(ano,4)+" y totales"
       @ 2,1 SAY "   ============================================="
      
       go top

       wfimporte=0
       wlimporte=0
       wpimporte=0
       weimporte=0
       do while .not. eof()
               if (year(fecha)=ano).and.(month(fecha)=MES)
                   @ PROW()+1,1 SAY cliente
                   @ PROW(),12 SAY FACTURA
                   @ PROW(),18 SAY FECHA
                   @ PROW(),27 SAY STR(IMPORTE,10,2)
                   @ PROW(),38 SAY MONEDA
                   @ PROW(),44 SAY FORMAPAGO
                   do case
                     case (moneda="PTS").or.(moneda="pts").or.(moneda=" ")
                        wpimporte=importe+wpimporte
                     case (moneda="FF")
                        wfimporte=importe+wfimporte
                     case moneda="LIT"
                        wlimporte=importe+wlimporte
                     case moneda="EUR"
                        weimporte=importe+weimporte
                   endcase
               endif
               skip
               IF PROW()+1>71
                  if xx="I" 
                   EJECT
                  endif
               ENDIF
       enddo
       IF PROW()+1>66
          if xx="I"
           EJECT
          endif
       ENDIF
      
       @ PROW()+4,1 SAY "Pesetas:  "+STR(wpimporte,14,2)
       @ PROW()+1,1 SAY "Liras:    "+STR(wlimporte,14,2)+" son "+STR(wlimporte/100*8.5,14,2)+" pts" 
       @ PROW()+1,1 SAY "Francos:  "+STR(wfimporte,14,2)+" son "+STR(wfimporte*25.25,14,2)+" pts"
       @ PROW()+1,1 SAY "Euros:    "+STR(weimporte,14,2)+" son "+STR(weimporte*166.386,14,2)+" pts"
       resultado=wpimporte+wlimporte/100*8.59+wfimporte*25.25+weimporte*166.386
       @ PROW()+2,1 SAY "Total:    "+STR(resultado,14,2)
         do case
		   	case xx="I"
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       modify file archivo.txt noedit
		        
		       delete file archivo.txt
		  endcase
       
       
       SET CENTURY ON
       WAIT WINDOW
RETURN

**********************************************
*********** PROCEDIMIENTO CONSULTA CLIENTES
***********************************************
PROCEDURE CONSULCLI
PARAMETERS SALIDA
       use clientes index innombre,inclave,innif
       store space(40) to wnombre
       clear
       @ 6,15 say "Introduce el cliente: " get wnombre
       read
       wprueba=trim(wnombre)
       wprueba=upper(wprueba)
       seek wprueba
       browse fields nombre,POBLACION,NIF NOMENU NOAPPEND noedit nodelete
       clear

     wclave=clave
     wformapago=formapago

     clear
     resp="a"
     @ 10,10 say "¿(A)nual, (m)ensual, o (t)otal?" get resp
     read
     resp=lower(resp)
     STORE SPACE(3) TO DISCO1
do case
 case resp="a"
     ano=1998
     clear
     @ 10,10 say "Introduzca el año       " get ano
     read
          
    
     clear
     XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    delete file archivo.txt
	    SET DEVICE TO file archivo.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
     
     set printer font "courier new",10
     SET CENTURY OFF
     @ 1,1 say "Facturacion durante el año  "+str(ano,4)+"  "+nombre
     @ 2,1 say "------------------------------------------------------------"
   
     use facturas index inclife,infac

     seek wclave
     if .not.found()
         wclave=" "
         wmoneda=" "
     endif
     wimporte=0
     @ prow()+1,1 say ""
     do while cliente=wclave.and.(.not.eof())
        if year(fecha)=ano

           if pagado
                 wpagado="pagado"
                 else
                 wpagado="impag."
           endif
           @ prow()+1,1 say fecha
           @ prow(),10 say factura
           @ prow(),20 say wpagado
           @ prow(),30 say str(importe,10,2)
           @ prow(),41 say moneda
           @ PROW(),51 say wformapago
             DISC01=MONEDA
           wimporte=wimporte+importe
        endif
           skip
           if prow()+1>71
             if xx="I"
              eject
             endif
           endif   
     enddo
     SKIP -1
     if prow()+1>66
         if xx="I" 
          eject
         endif
     endif
     @ prow()+2,1 say "Total:        "+str(wimporte,10,2)+" "+DISCO1
       do case
		   	case xx="I"
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       modify file archivo.txt noedit
		        
		       *delete file archivo.txt
		  endcase
     
     SET CENTURY ON
     WAIT WINDOW
 case resp="m"
     clear
     mes=1
     ano=1998
     @ 10,10 say "Mes del listado" get mes
     @ 12,10 say "Año del listado" get ano
     read
     clear
     XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    delete file archivo.txt
	    SET DEVICE TO file archivo.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
     set print font "courier new",10
     SET CENTURY OFF
     wmes=str(mes,2)
     wano=str(ano,4)
     @ 1,1 say "Facturacion durante el mes "+wmes+" de "+wano+"  "+nombre
     @ 2,1 say "--------------------------"
     
     use facturas index inclife,infac
     seek wclave
     wimporte=0
     @ prow()+1,1 say ""
     do while cliente=wclave
        if year(fecha)=ano.and.month(fecha)=mes
           if pagado
                 wpagado="pagado"
                 else
                 wpagado="impag."
           endif
           @ prow()+1,1 say fecha
           @ prow(),10 say factura
           @ prow(),20 say wpagado
           @ prow(),30 say str(importe,10,2)
           @ prow(),41 say moneda
           @ PROW(),51 say wformapago
              DISCO1=MONEDA
           wimporte=wimporte+importe
        endif
           skip
           if prow()+1>71
              if xx="I"
               eject
              endif
           endif
     enddo
     if prow()+1>66
       if xx="I" 
        eject
       endif
     endif
     @ prow()+2,1 say "Total:        "+str(wimporte,10,2)+" "+DISCO1
       do case
		   	case xx="I"
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       modify file archivo.txt noedit
		        
		       *delete file archivo.txt
		  endcase
     
     SET CENTURY ON
     WAIT WINDOW
 
 CASE resp="t" 
             
     XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    DELETE FILE archivo.txt
	    SET DEVICE TO file archivo.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR
     set print font "courier new",10
     SET CENTURY OFF
    
     @ 1,1 say "Facturacion total          "+nombre
     @ 2,1 say "---------------------------------------------"
     *A=3
     
     use facturas index inclife,infac
     seek wclave
     wimporte=0
    IF XX="I"
        @ prow()+1,1 say ""
    ENDIF
    
    do while cliente=wclave
           if pagado
                 wpagado="pagado"
                 else
                 wpagado="impag."
           endif
           IF XX="I"
           
	           @ prow()+1,1 say fecha
	           @ prow(),10 say factura
	           @ prow(),20 say wpagado
	           @ prow(),30 say str(importe,10,2)
	           @ prow(),41 say moneda
	           @ PROW(),51 say wformapago
           ELSE
	           *A=A+1
	           
	           @ ROW()+1,1 say fecha
	           
	           @ ROW(),10 say factura
	           @ ROW(),20 say wpagado
	           @ ROW(),30 say str(importe,10,2)
	           @ ROW(),41 say moneda
	           @ row(),51 say wformapago           
               
           ENDIF 
              DISCO1=MONEDA
           DO CASE
                 CASE MONEDA="EUR"
                    wimporte=wimporte+importe
                 CASE MONEDA="LIT"
                    * resultado=wpimporte+wlimporte/100*8.59+wfimporte*25.25+weimporte*166.386
                    WIMPORTE=WIMPORTE+IMPORTE/100*8.59/166.386
                 CASE MONEDA="FF"
                    WIMPORTE=WIMPORTE+IMPORTE*25.25/166.386 
                 OTHERWISE
                    wimporte=wimporte+importe/166.386
           ENDCASE
           skip
           if prow()+1>71
              if xx="I"
               eject
              endif
           endif
     enddo
     if prow()+1>66
       if xx="I" 
        eject
       endif
     endif
     @ prow()+2,1 say "Total:        "+str(wimporte,10,2)+" "+"EUR"
       do case
		   	case xx="I"
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       *IF A<=1820
		       modify file archivo.txt noedit
		       *ENDIF 
		       *delete file archivo.txt
		       
		  endcase
     
     SET CENTURY ON
     WAIT WINDOW
CASE resp="o"
	SET DEFAULT TO C:\VISPROGFAC2
	SET DELETED ON

	DIMENSION IMPORTECLIENTES(824,2) AS STRING 
	USE FACTURAS INDEX INCLIFE exclusive
	wano=1997
	CLEAR
	@ 1,1 say "Introducir año: " get wano
	read
	
	SET FILTER TO YEAR(FECHA)=wano
	WIMPORTE=0
	I=1
	GO TOP
	DO WHILE .NOT.EOF()

	  WCLIENTE=CLIENTE
	  WIMPORTE=0
	  DO WHILE CLIENTE=WCLIENTE
	      * WIMPORTE=WIMPORTE+IMPORTE
	           DO CASE
	                 CASE MONEDA="EUR"
	                    wimporte=wimporte+importe
	                 CASE MONEDA="LIT"
	                    * resultado=wpimporte+wlimporte/100*8.59+wfimporte*25.25+weimporte*166.386
	                    WIMPORTE=WIMPORTE+IMPORTE/100*8.59/166.386
	                 CASE MONEDA="FF"
	                    WIMPORTE=WIMPORTE+IMPORTE*25.25/166.386 
	                 OTHERWISE
	                    wimporte=wimporte+importe/166.386
	           ENDCASE
	           SKIP
	  
	  ENDDO
	  
	  
	  IMPORTECLIENTES(I,1)=WCLIENTE
	  IMPORTECLIENTES(I,2)=STR(WIMPORTE,10,2)
	  *?IMPORTECLIENTES(I,1)
	  *?IMPORTECLIENTES(I,2)
	  
	  
	  
	  I=I+1
	 
	ENDDO  
	  USE IMPORTECLIENTES index inimporte
	  DELETE ALL
	  PACK
	  APPEND FROM ARRAY IMPORTECLIENTES FOR importeclientes(i,2)<>0    
	  BROWSE
	  *INDEX ON IMPORTE TO INIMPORTE 
	  wresp="n"
	  CLEAR
	  *@ 1,1 say "¿desea imprimir? s/n" get wresp
	  *READ
	  *wresp=LOWER(wresp)
	  wresp=STR(MESSAGEBOX("¿desea imprimir?",4),1)
	  IF wresp="6"
		  SET PRINTER ON PROMPT
		  SET PRINTER FONT "COURIER NEW",9
		  SET DEVICE TO PRINT
		  SCAN
		    @ PROW()+1,1 SAY CLIENTE+"  "+IMPORTE
		    SKIP
		    @ PROW(),30 SAY CLIENTE+"  "+IMPORTE
		    SKIP
		    @ PROW(),60 SAY CLIENTE+"  "+IMPORTE
		    
		    IF PROW()+1>83
		       EJECT
		     ENDIF
		 ENDSCAN   
		 SET PRINTER OFF
		 SET PRINTER TO
      endif
 
 endcase
RETURN

**********************************
************FACTURA con comision
***********************************
PROCEDURE comision
*********ENTRADA DE CONCEPTOS

if respuesta<>"i"
wmodalidad="c"

wfecha=CTOD("  -  -  ")
store space(20) to wmatricula
store space(15) to worigen
store space(15) to wdestino1
wkilometros=0
wpreckilom=0
store space(10) to wreferencia
store space(3) to wmoneda
STORE SPACE(16) TO WTRANSPORT
store space(10) to wconductor
STORE SPACE(10) to wmcabeza
CLEAR
wtotal=0
timporte=0
dac=100

wnumero=0



use concepto INDEX INNUMERO,INFACFEC,in1ref


GO BOTTOM
orden=0
WNUMERO=NUMERO+1
ORDEN=ORDEN+1

do while dac<>0




       wfecha=CTOD("  -  -  ")
       COMPROBACION=WFECHA
       store space(20) to wmatricula
       store space(15) to worigen1

       store space(15) to worigen2
       store space(15) to wdestino1

       store space(15) to wdestino2
       store space(15) to wdestino3
       store space(15) to wdestino4
       store space(15) to wdestino5
       wprecton=0
       wtoneladas=0
       wcomision=0
       wkilometros=0
       wpreckilom=0
       store space(10) to wreferencia
       wmoneda="EUR"
       STORE SPACE(16) TO WTRANSPORT
       store space(10) to wconductor
       STORE SPACE(10) to wmcabeza
       store space(38) to wnotas

       ***CABECERA
       @ 1,10  say "Nombre: " +wnombre + " Modalidad:"+wMODALIDAD
       @ 2,10  say "Factura: "+wnumfac+"               "+" Fecha: "+dtoc(fechafac)
       @ 3,10  say "Forma de pago: " +wnformapago

       @ 4,10  say "Referencia: "+str(wnumero,7) +"  Numero de orden: "+str(orden,4)
       ****INTRODUCCION DE CONCEPTOS
       *CLEAR GETS


       @ 6,20  say "Fecha :           " get wfecha
       @ 7,20  say "Matricula :       " get wmatricula
       @ 8,20  say "Origen 1:         " get worigen1
       @ 9,20  say "Origen 2:         " get worigen2
       @ 10,20 say "Destino 1:        " get wdestino1
       @ 11,20 say "Destino 2:        " get wdestino2
       @ 12,20 say "Destino 3:        " get wdestino3
       @ 13,20 say "Destino 4:        " get wdestino4
       @ 14,20 say "% comision:       " get wcomision
       @ 15,20 say "Toneladas  :      " get wtoneladas picture "99.99"
       @ 16,20 say "Precio/tonelada : " get wprecton PICTURE "99,999.99"
       @ 17,20 say "Nº Referencia     " get wreferencia
       @ 18,20 say "Moneda:           " get wmoneda
       @ 20,20 say "Transportista:    " get wtransport
       @ 21,20 say "Conductor:        " get wconductor
       @ 22,20 say "Matricula cabeza: " get wmcabeza
       @ 23,20 say "Notas:            " get wnotas
       read
       wimporte=wtoneladas*wprecton
       wimporte=wimporte-wimporte/100*wcomision
       wtotal=wtotal+wimporte



     IF  DTOC(WFECHA)<>"  -  -  "




       append blank

       replace fecha      with wfecha
       replace matricula  with wmatricula
       replace origen1    with worigen1
       replace origen2    with worigen2
       replace destino1   with wdestino1

       replace destino2   with wdestino2
       replace destino3   with wdestino3
       replace destino4   with wdestino4
       replace comision   with wcomision
       replace toneladas  with wtoneladas
       replace precton    with wprecton
       replace referencia with wreferencia
       replace numfac     with wnumfac
       replace moneda     with wmoneda
       replace cliente    with wclave
       replace numero     with wnumero
       replace conductor  with wconductor
       replace transport  with wtransport
       replace mcabeza    with wmcabeza
       replace notas      with wnotas

       wnumero=numero+1
       orden=orden+1
    ELSE
    ENDIF
    a="s"
    @ 24,35 say "(s)seguir /(t) terminado. " GET A
    read
    dac=asc(a)
    do case
       case (dac=116).or.(dac=84)
          dac=0
    endcase
enddo

clear
respuesta="n"
store space(38) to wnota1,wnota2,wnota3,wnota4,wnota5,wnota6,wnota7
@ 20,10 say "¿Desea incluir alguna nota en la factura?" get respuesta
read
respuesta=lower(respuesta)
if respuesta="s"
       clear
       @ 10,20 say "Linea 1: " get wnota1

       @ 11,20 say "Linea 2: " get wnota2
       @ 12,20 say "Linea 3: " get wnota3
       @ 13,20 say "Linea 4: " get wnota4
       @ 14,20 say "Linea 5: " get wnota5

       @ 15,20 say "Linea 6: " get wnota6
       @ 16,20 say "Linea 7: " get wnota7
       read
endif



**INTRODUCCION DE DATOS EN EL FICHERO FACTURAS
use facturas index infac,inclife
*indexada por numero de factura
append blank
replace envfecha with fechaenv
replace fecha   with fechafac
replace factura with wnumfac
iva=(wtotal/100)*16
finaltotal=iva+wtotal
replace importe with finaltotal
replace cliente with wclave
wpagado=.f.
replace pagado  with wpagado
REPLACE MONEDA WITH WMONEDA
replace formapago with wnformapago

replace nota1 with wnota1
replace nota2 with wnota2
replace nota3 with wnota3
replace nota4 with wnota4
replace nota5 with wnota5
replace nota6 with wnota6
replace nota7 with wnota7
replace modalidad with wmodalidad
endif
***fin de respuesta<>"i"


*********************************
*******IMPRESION DE LA FACTURA


***IMPRESION DE com
XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file archivo.txt
	   ELSE 
	    SET PRINT ON prompt
	    set device to print
	
	
	ENDIF
	CLEAR


SET CENTURY OFF
SET PRINTER FONT "COURIER NEW",11
*impresion de la direccion del cliente

@ 11,8 SAY WNOMBRE
@ 12,8 SAY WDIRECCION

@ 13,8 SAY WPOLIGONO
@ 14,8 SAY WCODPOSTAL+" "+LEFT(WPOBLACION,20)+" "+WPROVINCIA

@ 15,8 SAY WPAIS

@ 18,56 SAY "NIF "+WNIF

SET CENTURY ON
@ 23,14 SAY WNUMFAC
@ 23,67 SAY FECHAFAC
SET CENTURY OFF

@ 25,4 SAY "FORMA DE PAGO: "+WNFORMAPAGO
SET PRINT FONT "COURIER NEW",10

********* APERTURA DEL FICHERO CONCEPTOS E IMPRESION DE ESTOS
use concepto index infacfec,innumero,in1ref

seek wnumfac
if respuesta="i"
       finaltotal=0
endif
indice=0
do  while numfac=wnumfac
       resultado=transform(toneladas*precton,"99,999.99")
      
       RESUL1=STR(TONELADAS,5,2)

       

       APRECTON=PRECTON
       cprecton=str(Aprecton,5,2)

       
       @ 34,1 SAY " "
       @ PROW()+1,3 SAY DTOC(FECHA)
       @ PROW(),13 SAY LEFT(MATRICULA,10)
       @ PROW(),25 SAY LEFT(ORIGEN1,15)+" "+LEFT(DESTINO1,13)+" "+RESUL1+"Tn"
       @ PROW(),66 SAY CPRECTON
       @ PROW(),78 SAY RESULTADO
       coin=moneda
       if (origen2<>" ").or.(destino2<>" ")
         @ PROW()+1 ,25 SAY LEFT(ORIGEN2,15)+"-"+DESTINO2
         
       endif
       if destino3<>" "
           @ PROW()+1,42 SAY DESTINO3
       endif
       if destino4<>" "
           @ PROW()+1,42 SAY DESTINO4
       endif
       if destino5<>" "
           @ PROW()+1,42 SAY DESTINO5
       endif
       if notas<>" "
           @ PROW()+1,25 SAY NOTAS
       endif
       wcomision=0
       if comision<>0
           WCOMISION=toneladas*precton/100*COMISION

       resultado=transform(wcomision,"99,999.99")
       
           @ PROW()+1,25 SAY "COMISION "+STR(COMISION,2)+"%................"
           @ PROW(),78 SAY RESULTADO
       ENDIF
      neto=toneladas*precton-WCOMISION
       resultado=transform(neto,"99,999.99")
      
       @ PROW()+1,78 SAY "  ----------"
       @ PROW()+1,78 SAY RESULTADO
       indice=indice+1

       if respuesta="i"
                finaltotal=finaltotal+toneladas*precton-WCOMISION
       endif

       skip
enddo
?

*************IMPRESION POR 1ª VEZ

if respuesta<>"i"
       finaltotal=finaltotal-iva
endif
if indice>1
     resultadototal=transform(finaltotal,"99,999.99")

      
     @ PROW()+1,78 SAY "---------"
     @ PROW()+1,78 SAY RESULTADOTOTAL
endif
if respuesta="i"
       iva=0
       iva=(finaltotal)/100*16
endif
resultadoiva=transform(iva,"99,999.99")

      





@ PROW()+1,66 SAY "16% IVA"
@ PROW(),78 SAY RESULTADOIVA
@ PROW()+1,78 SAY "---------"
if respuesta="i"
    finaltotal=finaltotal+iva
endif
if respuesta<>"i"
       finaltotal=finaltotal+iva
endif
resultadofinal=transform(finaltotal,"99,999.99")

      

if coin="EUR"

        @ PROW()+1,66 SAY "TOTAL.."
        @ PROW(),78 SAY RESULTADOFINAL+" EUR"
      else

        @ PROW()+1,66 SAY "TOTAL.."
        @ PROW(),78 SAY RESULTADOFINAL
endif
*?

***************************
******* tratamiento de euros
*****************************
euros=finaltotal/166.386
reuros=transform(euros,"999,999,999.99")

      



IF coin<>"EUR"
************************************
*******tratamiento de euros
?replicate(" ",67),"        ",reuros,"EUR"
*******************+

?replicate(" ",78),"========="
ENDIF
*******impresion de notas

       @ PROW()+3,25 SAY WNOTA1 
       @ PROW()+1,25 SAY WNOTA2
       @ PROW()+1,25 SAY WNOTA3
       @ PROW()+1,25 SAY WNOTA4
       @ PROW()+1,25 SAY WNOTA5
       @ PROW()+1,25 SAY WNOTA6
       @ PROW()+1,25 SAY WNOTA7
SET CENTURY ON
          do case
		   	case xx="I"
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       modify file archivo.txt noedit
		        
		       delete file archivo.txt
		  endcase

WAIT WINDOW
RETURN









*********************************************
************PROCEDIMIENTO COPIA SEGURIDAD
**************************************************
procedure copseg
clear
@ 5,10 say "             C-o-p-i-a---d-e---s-e-g-u-r-i-d-a-d"
@ 10,10 say "Introduzca el disquete y pulse una tecla"
wait WINDOW " "




run copy concepto.dbf a:
run copy facturas.dbf a:
run copy clientes.dbf a:
return

*******************************************
********MANTENIMIENTO DE CLIENTES
************************************

PROCEDURE MANCLI
volver=" "
SEGUIR=.T.
do while SEGUIR
  clear
  @ 1,1 TO 28,94
  @ 5,10 SAY "MANTENIMIENTO DE CLIENTES"
  @ 7,5 TO 24,90 DOUBLE
  @ 10,10 say "(A)ltas"
  @ 12,10 say "(B)ajas"
  @ 14,10 say "(M)odificaciones"
  @ 16,10 say "(C)onsultas"
  @ 18,10 say "Consulta por c(l)ave"
  @ 20,10 say "Consulta por (N)IF"
  @ 22,10 say "(V)olver        " get volver
  read
  volver=lower(volver)
  do case
       case volver="a"

            use clientes index innombre,inclave,innif

            store space(40) to wnombre
            store space(40) to wdireccion
            store space(6)  to wcodpostal
            store space(30) to wpoblacion
            store space(10) to wprovincia
            store space(10) to wpais
            store space(16) to wnif
            store space(10) to wclave
            store space(40) to wdepartamen
            store space(40) to wpoligono
            store space(16) to wtelefono
            store space(10) to wapdo
            store space(10) to wcontacto
            store space(12) to wactividad
            store space(50) to wformapago
            clear
            @ 6,5 say  "Nombre:        " get wnombre
            @ 7,5 say  "Direccion:     " get wdireccion
            @ 8,5 say  "Codigo Postal: " get wcodpostal
            @ 9,5 say  "Poblacion:     " get wpoblacion
            @ 10,5 say "Provincia:     " get wprovincia
            @ 11,5 say "Pais:          " get wpais
            @ 12,5 say "Nif:           " get wnif
            @ 13,5 say "Clave:         " get wclave
            @ 14,5 say "Departamento:  " get wdepartamen
            @ 15,5 say "Poligono:      " get wpoligono
            @ 16,5 say "Telefono:      " get wtelefono
            @ 17,5 say "Apartado:      " get wapdo
            @ 18,5 say "Contacto:      " get wcontacto
            @ 19,5 say "Actividad:     " get wactividad
            @ 20,5 say "Forma de pago: " get wformapago
            read
            wnformapago=wformapago
            IF WCLAVE=" "
              CLEAR

               @ 13,5 say "Es necesaria la clave: " get wclave
              READ
            ENDIF
            append blank
            replace nombre with wnombre
            replace direccion with wdireccion
            replace codpostal with wcodpostal
            replace poblacion with wpoblacion
            replace provincia with wprovincia
            replace pais with wpais
            replace nif with wnif
            replace clave with wclave
            replace departamen with wdepartamen
            replace poligono with wpoligono
            replace telefono with wtelefono
            replace apdo with wapdo
            replace contacto with wcontacto
            replace actividad with wactividad
            replace formapago with wformapago

       case volver="b"
            use clientes index innombre,inclave,innif
            clear
            store space(40) to wnombre
            @ 10,5 say "Nombre: " get wnombre
            read
            wnombre=upper(wnombre)
            wnombre=trim(wnombre)
            seek wnombre
            browse fields nombre,poblacion,NIF noappend nomenu noedit nodelete
            clear
            *@ 10,5 say " " +nombre
            *wrespuesta="n"
            *@ 11,5 say "¿Esta seguro de que desea borrarlo? s/n" get wrespuesta
            *read
            *wrespuesta=lower(wrespuesta)
            wrespuesta=STR(MESSAGEBOX(nombre+CHR(13)+"¿Esta seguro de que desea borrarlo?",4),1)
            if wrespuesta="6"
                    delete
                    WAIT WINDOW "El cliente ha sido borrado"
            endif
       case volver="m"


            use clientes index innombre,inclave,innif
            clear
            store space(40) to wnombre
            @ 10,5 say "Nombre: " get wnombre
            read
            wnombre=upper(wnombre)
            wnombre=trim(wnombre)
            seek wnombre
            browse fields nombre,poblacion,NIF noappend nomenu noedit nodelete
            clear
            @ 6,5 say  "Nombre:        " get nombre
            @ 7,5 say  "Direccion:     " get direccion
            @ 8,5 say  "Codigo Postal: " get codpostal
            @ 9,5 say  "Poblacion:     " get poblacion
            @ 10,5 say "Provincia:     " get provincia
            @ 11,5 say "Pais:          " get pais
            @ 12,5 say "Nif:           " get nif
            @ 13,5 say "Clave:         " +clave
            @ 14,5 say "Departamento:  " get departamen
            @ 15,5 say "Poligono:      " get poligono
            @ 16,5 say "Telefono:      " get telefono
            @ 17,5 say "Apartado:      " get apdo
            @ 18,5 say "Contacto:      " get contacto
            @ 19,5 say "Actividad:     " get actividad
            @ 20,5 say "Forma de pago: " get formapago
            read

       case volver="c"

            use clientes index innombre,inclave,innif
            store space(40) to wnombre
            clear
            @ 6,15 say "Introduce el cliente: " get wnombre
            read
            wprueba=trim(wnombre)
            wprueba=upper(wprueba)
            seek wprueba
            wclave=clave
            browse fields nombre,POBLACION,NIF NOMENU NOAPPEND noedit nodelete
            clear

     @ 3,10 say  "Nombre:        " +nombre
     @ 4,10 say  "Direccion:     " +direccion
     @ 5,10 say  "Codigo Postal: " +codpostal
     @ 6,10 say  "Poblacion:     " +poblacion
     @ 7,10 say  "Provincia:     " +provincia
     @ 8,10 say  "Pais:          " +pais
     @ 9,10 say  "Nif:           " +nif
     @ 10,10 say "Clave:         " +clave
     @ 11,10 say "Departamento:  " +departamen
     @ 12,10 say "Poligono:      " +poligono
     @ 13,10 say "Telefono:      " +telefono
     @ 14,10 say "Apartado:      " +apdo
     @ 15,10 say "Contacto:      " +contacto
     @ 16,10 say "Actividad:     " +actividad
     @ 17,10 say "Forma de pago: " +formapago
            ?
            ?
            WAIT WINDOW
            
            WS="N"
            CLEAR
            @ 1,1 SAY "¿IMPRIMIR SOBRE S/N?   "  GET WS
            READ
            WS=LOWER(WS)
            IF WS="s"
            
                SELECT * FROM CLIENTES WHERE clave=wclave INTO CURSOR CURSORSOBRES
                REPORT FORM informedireccionsobre TO PRINTER prompt preview
                CLOSE INDEXES
                CLOSE DATABASES 
                *WAIT window
            ENDIF
            

       case volver="l"

            use clientes index inclave,innombre,innif
            store space(10) to wclave
            clear
            @ 6,15 say "Introduce la clave del cliente: " get wclave
            read
            wprueba=trim(wclave)
            wprueba=upper(wprueba)
            seek wprueba
            browse fields clave,POBLACION,NIF NOMENU NOAPPEND noedit nodelete
            clear

     @ 3,10 say  "Nombre:        " +nombre
     @ 4,10 say  "Direccion:     " +direccion
     @ 5,10 say  "Codigo Postal: " +codpostal
     @ 6,10 say  "Poblacion:     " +poblacion
     @ 7,10 say  "Provincia:     " +provincia
     @ 8,10 say  "Pais:          " +pais
     @ 9,10 say  "Nif:           " +nif
     @ 10,10 say "Clave:         " +clave
     @ 11,10 say "Departamento:  " +departamen
     @ 12,10 say "Poligono:      " +poligono
     @ 13,10 say "Telefono:      " +telefono
     @ 14,10 say "Apartado:      " +apdo
     @ 15,10 say "Contacto:      " +contacto
     @ 16,10 say "Actividad:     " +actividad
     @ 17,10 say "Forma de pago: " +formapago
            ?
            ?
            WAIT WINDOW

        case volver="n"
             use clientes index innif,inclave,innombre
             store space(9) to wnif
            clear
            @ 6,15 say "Introduce el NIF del cliente: " get wnif
            read
            wprueba=trim(wnif)
            wprueba=upper(wprueba)
            seek wprueba
            browse fields nif,nombre,POBLACION,NIF NOMENU NOAPPEND noedit nodelete
            clear

     @ 3,10 say  "Nombre:        " +nombre
     @ 4,10 say  "Direccion:     " +direccion
     @ 5,10 say  "Codigo Postal: " +codpostal
     @ 6,10 say  "Poblacion:     " +poblacion
     @ 7,10 say  "Provincia:     " +provincia
     @ 8,10 say  "Pais:          " +pais
     @ 9,10 say  "Nif:           " +nif
     @ 10,10 say "Clave:         " +clave
     @ 11,10 say "Departamento:  " +departamen
     @ 12,10 say "Poligono:      " +poligono
     @ 13,10 say "Telefono:      " +telefono
     @ 14,10 say "Apartado:      " +apdo
     @ 15,10 say "Contacto:      " +contacto
     @ 16,10 say "Actividad:     " +actividad
     @ 17,10 say "Forma de pago: " +formapago
            ?
            ?
            WAIT WINDOW
        
        
        case volver="v"

                 
                 SEGUIR=.F.
     endcase
enddo











RETURN




**********************************
********* RESULTADO DE CAMIONES
*********************************
PROCEDURE RESULCAM
private wcamion,mes,tlimporte,tfimporte,tpimporte
tlimporte=0
tfimporte=0
tpimporte=0
TEIMPORTE=0
mes=1
Año=2005


store space(10) to wcamion

CLEAR
@ 10,10 say "Camion: " get wcamion
@ 11,10 say "Mes:    " get mes
@ 12,10 SAY "Año:    " get año
read

WCAMION=UPPER(WCAMION)
use concepto
index on mcabeza+str(year(fecha),4)+str(month(fecha),2)+str(day(fecha),2) to intemp1
WMES=CTOD("01-"+STR(MES,2)+"-"+str(año,4))
XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	_screen.FontSize=10
	XX=UPPER(XX)
	IF XX="P"
	    
		?"            ",wcamion,"   ",cmonth(Wmes),"   ",year(Wmes)
		?"-------------------------------------------------------------------"
		?
		seek wcamion
		do while mcabeza=wcamion
		       if month(fecha)=mes.and.year(fecha)=año
		            ?LEFT(CDOW(FECHA),3),str(day(fecha),2),conductor,LEFT(MATRICULA,10),origen1,destino1,IMPORTE,moneda,cliente
		            do case
		            case moneda="LIT"
		                tlimporte=tlimporte+importe
		            case moneda="FF"
		                tfimporte=tfimporte+importe
		            case moneda="PTS".or.moneda=" "
		                tpimporte=tpimporte+importe
		            CASE MONEDA="EUR"
		                TEIMPORTE=TEIMPORTE+IMPORTE
		            endcase
		       endif
		       SKIP
		enddo
		?
		ptlimporte=tlimporte/1936.27*166.386
		?"Importe en liras:   ",STR(tlimporte,10),"lit.",STR(ptlimporte,10,2),"pts."
		ptfimporte=tfimporte/6.55957*166.386
		?"Importe en francos: ",STR(tfimporte,10,2),"ff. ",STR(ptfimporte,10,2),"pts."
		?"Importe en pesetas: ",STR(tpimporte,10,2),"pts."

		?"Importe en EUROS: ",STR(tEimporte,10,2),"EUR."
		ptotal=ptlimporte+ptfimporte+tpimporte+TEIMPORTE*166.386
		?"Importe total:      ",STR(ptotal,10,2),"pts.",STR(PTOTAL/166.386,10,2),"EUR"
		CLOSE INDEX
		DELETE FILE intemp1.IDX
	    WAIT WINDOW
	 ELSE 
	    SET PRINT ON prompt
		?"            ",wcamion,"   ",cmonth(Wmes),"   ",year(Wmes)
		?"-------------------------------------------------------------------"
		?
		seek wcamion
		do while mcabeza=wcamion
		       if month(fecha)=mes.and.year(fecha)=año
		            ?LEFT(CDOW(FECHA),3),str(day(fecha),2),conductor,LEFT(MATRICULA,10),origen1,destino1,IMPORTE,moneda,cliente
		            do case
		            case moneda="LIT"
		                tlimporte=tlimporte+importe
		            case moneda="FF"
		                tfimporte=tfimporte+importe
		            case moneda="PTS".or.moneda=" "
		                tpimporte=tpimporte+importe
		            CASE MONEDA="EUR"
		                TEIMPORTE=TEIMPORTE+IMPORTE
		            endcase
		       endif
		       SKIP
		enddo
		?
		ptlimporte=tlimporte/1936.27*166.386
		?"Importe en liras:   ",STR(tlimporte,10),"lit.",STR(ptlimporte,10,2),"pts."
		ptfimporte=tfimporte/6.55957*166.386
		?"Importe en francos: ",STR(tfimporte,10,2),"ff. ",STR(ptfimporte,10,2),"pts."
		?"Importe en pesetas: ",STR(tpimporte,10,2),"pts."

		?"Importe en EUROS: ",STR(tEimporte,10,2),"EUR."
		ptotal=ptlimporte+ptfimporte+tpimporte+TEIMPORTE*166.386
		?"Importe total:      ",STR(ptotal,10,2),"pts.",STR(PTOTAL/166.386,10,2),"EUR"
		CLOSE INDEX
		DELETE FILE intemp1.IDX
		SET PRINT OFF
		set printer to
	
	ENDIF
	CLEAR
 _screen.FontSize=12

*WAIT
RETURN

*********************************
****** procedimento AGUADO
*********************************

PROCEDURE AGUADO
PARAMETERS MES,ano
       SELECT 1
       use facturas index inFAC,inclife
       SELECT 2
       USE CLIENTES INDEX INCLAVE,INNOMBRE,INNIF
       XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file archivo.txt
	   ELSE 
	    CLEAR 
	    @ 1,1 SAY "PULSA PREFERENCIAS EN EL CONTROLADOR DE LA IMPRESORA"
	    @ 2,1 SAY "Y ELIJE HORIZONTAL"
	    WAIT WINDOW
	    SET PRINT ON prompt
	    set device to print
	    SET PRINT FONT "COURIER NEW",9
	
	ENDIF
	CLEAR
       
       
       
       SET CENTURY OFF
       wfecha=ctod("01-"+str(mes,2)+"-"+str(ano,4))
       @ 1,1 SAY "    Facturas de "+cmonth(wfecha)+" de "+str(ano,4)+" y totales"
       @ 2,1 SAY "   ============================================="
       
       SELECT 1
       go top

       wfimporte=0
       wlimporte=0
       wpimporte=0
       weimporte=0
       
       do while .not. eof()
               if (year(fecha)=ano).and.(month(fecha)=MES)
                   PRUEBA=" "
                   PRUEBA=CLIENTE
                   SELECT 2
                   SEEK(PRUEBA)
                   WNOMBRE=NOMBRE
                   @ PROW()+1,1 SAY LEFT(NOMBRE,30)
                   @ PROW(),32 SAY NIF
                   @ PROW(),47 SAY LEFT(POBLACION,15)
                   @ PROW(),63 SAY LEFT(PROVINCIA,12)
                   
                   SELECT 1
                   @ PROW(),76 SAY FACTURA
                   @ PROW(),86 SAY FECHA
                   
                   TIPOIVA=" "
                   IVA=0
                   BASE=0
                   DO CASE
                      CASE MODALIDAD="r"
                              TIPOIVA="8%"
                              BASE=IMPORTE/1.08
                              IVA=IMPORTE-BASE
                      CASE MODALIDAD="8"
                              TIPOIVA="18%"
                              BASE=IMPORTE/1.18
                              IVA=IMPORTE-BASE       
                      CASE MODALIDAD="n".OR.MODALIDAD="o"
                              TIPOIVA="16%"
                              BASE=IMPORTE/1.16
                              IVA=IMPORTE-BASE
                      CASE modalidad="10"
                              tipoiva="10%"
                              base=importe/1.1
                              iva=importe-base
                      CASE modalidad="21".OR.MODALIDAD="m"
                              tipoiva="21%"
                              base=importe/1.21
                              iva=importe-base
                      CASE MODALIDAD="e"
                              TIPOIVA="0%"
                              BASE=IMPORTE
                              IVA=0
                   ENDCASE           
                   @ PROW(),96 SAY STR(BASE,10,2)
                   @ PROW(),108 SAY TIPOIVA
                   @ PROW(),111 SAY STR(IVA,10,2)
                   @ PROW(),121 SAY STR(IMPORTE,10,2)
                  
                   do case
                     case (moneda="PTS").or.(moneda="pts").or.(moneda=" ")
                        wpimporte=importe+wpimporte
                     case (moneda="FF")
                        wfimporte=importe+wfimporte
                     case moneda="LIT"
                        wlimporte=importe+wlimporte
                     case moneda="EUR"
                        weimporte=importe+weimporte
                   endcase
               endif
               skip
               IF PROW()+1>58
                   if xx="I"
                     EJECT
                   endif  
               ENDIF
       enddo
       IF PROW()+1>53
           if xx="I"
              EJECT
           endif
       ENDIF
       
       @ PROW()+4,1 SAY "Pesetas:  "+STR(wpimporte,14,2)
       @ PROW()+1,1 SAY "Liras:    "+STR(wlimporte,14,2)+" son "+STR(wlimporte/100*8.5,14,2)+" pts" 
       @ PROW()+1,1 SAY "Francos:  "+STR(wfimporte,14,2)+" son "+STR(wfimporte*25.25,14,2)+" pts"
       @ PROW()+1,1 SAY "Euros:    "+STR(weimporte,14,2)+" son "+STR(weimporte*166.386,14,2)+" pts"
       resultado=wpimporte+wlimporte/100*8.59+wfimporte*25.25+weimporte*166.386
       @ PROW()+2,1 SAY "Total:    "+STR(resultado,14,2)
         do case
		   	case xx="I"
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       modify file archivo.txt noedit
		        
		       delete file archivo.txt
		  endcase
       close indexes
       close databases
       
       SET CENTURY ON
       WAIT WINDOW

RETURN





*********************************
****** procedimento AGUADO ANUAL
*********************************

PROCEDURE AGUADOANUAL
PARAMETERS MES,ano
       SELECT 1
       use facturas index inFAC,inclife
       SELECT 2
       USE CLIENTES INDEX INCLAVE,INNOMBRE,INNIF
       XX="I"
	CLEAR
	@ 1,1 SAY "¿SALIDA POR (P)ANTALLA O (I)MPRESORA?" GET XX
	READ
	XX=UPPER(XX)
	IF XX="P"
	    
	    SET DEVICE TO file archivo.txt
	   ELSE 
	    CLEAR 
	    @ 1,1 SAY "PULSA PREFERENCIAS EN EL CONTROLADOR DE LA IMPRESORA"
	    @ 2,1 SAY "Y ELIJE HORIZONTAL"
	    WAIT WINDOW
	    SET PRINT ON prompt
	    set device to print
	    SET PRINT FONT "COURIER NEW",9
	
	ENDIF
	CLEAR
       
       
       
       SET CENTURY OFF
       wfecha=ctod("01-"+str(mes,2)+"-"+str(ano,4))
       @ 1,1 SAY "    Facturas de "+cmonth(wfecha)+" de "+str(ano,4)+" y totales"
       @ 2,1 SAY "   ============================================="
       
       SELECT 1
       go top

       wfimporte=0
       wlimporte=0
       wpimporte=0
       weimporte=0
       
       do while .not. eof()
               if (year(fecha)=ano).and.(month(fecha)=MES)
                   PRUEBA=" "
                   PRUEBA=CLIENTE
                   SELECT 2
                   SEEK(PRUEBA)
                   WNOMBRE=NOMBRE
                   @ PROW()+1,1 SAY LEFT(NOMBRE,30)
                   @ PROW(),32 SAY NIF
                   @ PROW(),47 SAY LEFT(POBLACION,15)
                   @ PROW(),63 SAY LEFT(PROVINCIA,12)
                   
                   SELECT 1
                   @ PROW(),76 SAY FACTURA
                   @ PROW(),86 SAY FECHA
                   
                   TIPOIVA=" "
                   IVA=0
                   BASE=0
                   DO CASE
                      CASE MODALIDAD="r"
                              TIPOIVA="8%"
                              BASE=IMPORTE/1.08
                              IVA=IMPORTE-BASE
                      CASE MODALIDAD="8"
                              TIPOIVA="18%"
                              BASE=IMPORTE/1.18
                              IVA=IMPORTE-BASE       
                      CASE MODALIDAD="n".OR.MODALIDAD="o"
                              TIPOIVA="16%"
                              BASE=IMPORTE/1.16
                              IVA=IMPORTE-BASE
                      CASE modalidad="10"
                              tipoiva="10%"
                              base=importe/1.1
                              iva=importe-base
                      CASE modalidad="21".OR.MODALIDAD="m"
                              tipoiva="21%"
                              base=importe/1.21
                              iva=importe-base
                      CASE MODALIDAD="e"
                              TIPOIVA="0%"
                              BASE=IMPORTE
                              IVA=0
                   ENDCASE           
                   @ PROW(),96 SAY STR(BASE,10,2)
                   @ PROW(),108 SAY TIPOIVA
                   @ PROW(),111 SAY STR(IVA,10,2)
                   @ PROW(),121 SAY STR(IMPORTE,10,2)
                  
                   do case
                     case (moneda="PTS").or.(moneda="pts").or.(moneda=" ")
                        wpimporte=importe+wpimporte
                     case (moneda="FF")
                        wfimporte=importe+wfimporte
                     case moneda="LIT"
                        wlimporte=importe+wlimporte
                     case moneda="EUR"
                        weimporte=importe+weimporte
                   endcase
               endif
               skip
               IF PROW()+1>58
                   if xx="I"
                     EJECT
                   endif  
               ENDIF
       enddo
       IF PROW()+1>53
           if xx="I"
              EJECT
           endif
       ENDIF
       
       @ PROW()+4,1 SAY "Pesetas:  "+STR(wpimporte,14,2)
       @ PROW()+1,1 SAY "Liras:    "+STR(wlimporte,14,2)+" son "+STR(wlimporte/100*8.5,14,2)+" pts" 
       @ PROW()+1,1 SAY "Francos:  "+STR(wfimporte,14,2)+" son "+STR(wfimporte*25.25,14,2)+" pts"
       @ PROW()+1,1 SAY "Euros:    "+STR(weimporte,14,2)+" son "+STR(weimporte*166.386,14,2)+" pts"
       resultado=wpimporte+wlimporte/100*8.59+wfimporte*25.25+weimporte*166.386
       @ PROW()+2,1 SAY "Total:    "+STR(resultado,14,2)
         do case
		   	case xx="I"
			   set print off
			   set printer to
			   set device to screen
		    case xx="P"
		       set device to screen
		       modify file archivo.txt noedit
		        
		       delete file archivo.txt
		  endcase
       close indexes
       close databases
       
       SET CENTURY ON
       WAIT WINDOW

RETURN







********************************
********************
**********  PROCEDIMIENTO ETIQUETAS
**************************************
*************************************
*funcion relleno de ceros padl





PROCEDURE ETIQUETAS

		             **********************************
		*************************
		****** OBTENCION DE LOS DATOS
		*************************
		**********************************
		*SET NULL OFF 
		

		WNUMERO=0
		CLOSE DATABASES


		CLEAR



		@ 1,1 SAY "NUMERO DE MERENDOLA:" GET WNUMERO

		READ
		
		 IF WNUMERO>999
		        CTABLA="[MMERENDOLA "+str(WNUMERO,4)+"]"
		     
		     else
		        CTABLA="[MERENDOLA "+STR(WNUMERO,3)+"]"
		 endif
		
		*CTABLA="[MERENDOLA "+STR(WNUMERO,3)+"]"
		*CTABLA="[MERENDOLA 600]"

		*a=SQLSTRINGCONNECT("Driver={Microsoft Access Driver (*.mdb)};Dbq=\\ALVARO-PC\Users\Alvaro\Desktop\SAPITO vacaciones.mdb;Uid=Admin;Pwd=;")
		a=SQLSTRINGCONNECT("Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\VISPROGFAC2\SAPITO vacaciones.mdb;Uid=Admin;Pwd=;")


		?a
		B=SQLEXEC(A,"SELECT * FROM "+CTABLA+" ORDER BY POSICION","CURSOR1")
		*B=SQLEXEC(A,"SELECT * FROM [merendola 600]","CURSOR1")
		?b
		*SELECT * FROM CURSOR1 INTO TABLE TABLA1
		*BROW
		*IF B<>-1
		DO WHILE .t.

		resp="p"
		CLEAR
		@ 1,1 say "(i)mprimir ,vista (p)revia, (s)alir" get resp
		READ
		resp=LOWER(resp)


		DO case
		     CASE resp="p"
		       LABEL FORM ETIQUETASMERENDOLA PREVIEW
		     CASE resp="i"
		       LABEL FORM etiquetasmerendola TO PRINTER prompt
		     CASE resp="s"
		       exit

		     ENDCASE

		ENDDO
		  
		       
		       
		*ENDIF
		SQLDISCONNECT(0)

        CLEAR


RETURN



***********************************
**********************************
********** PROCEDIMIENTO AGENCIA
**************************************
**************************************


*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

********   R E C O R D A R   P O S I B L E    E R R O R    D E    N U M E R A C I O N    D E    M E R E N D O L A S

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




PROCEDURE AGENCIA


		*CD "\\BELEN\VISPROGFAC2\FOXAGENCIA y etiquetas"

		**********************************
		*************************
		****** OBTENCION DE LOS DATOS
		*************************
		**********************************
		*SET NULL OFF 
		



		*a=SQLSTRINGCONNECT("Driver={Microsoft Access Driver (*.mdb)};Dbq=\\ALVARO-PC\Users\Alvaro\Desktop\SAPITO vacaciones.mdb;Uid=Admin;Pwd=;")
		*a=SQLSTRINGCONNECT("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=\\Alvaro-vaio\Users\alvaro\Documents\sapito vacaciones (4).mdb")
		a=SQLSTRINGCONNECT("Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\VISPROGFAC2\SAPITO vacaciones.mdb;Uid=Admin;Pwd=;")
		
		
		?A
		CTABLA="[MERENDOLA 600]"
		SET EXCLUSIVE ON
		USE tabla1 INDEX INTEMP
		*ZAP
		DELETE ALL
		PACK
		SET EXCLUSIVE OFF  

		CLOSE DATABASES



		MES=7
		anno=2006
		CLEAR 
		@ 5,5 say "Mes:    " get mes
		@ 6,5 say "Año:    " get anno
		READ




		NUM=600
		B=1

		DO WHILE B<>-1
		  
		  
		     B=SQLEXEC(A,"SELECT * FROM "+CTABLA,"CURSOR1")
		   
		   IF B<>-1
		     SELECT * FROM CURSOR1 INTO TABLE TABLA2
		     CLOSE DATABASES 
		     SELECT 1
		     USE TABLA2
		     SELECT 2
		     USE TABLA1 INDEX INTEMP
		     *APPEND FROM TABLA2 FOR ((LEFT(MATRICULA,7)="AGENCIA").AND.(MES=MONTH(FECHA_ORIG)))
		     WPOSICION=0
		     WPRECIO=0
		     WAGENCIA_PR=0
		     WAGENCIA_PO=0
		     
		     
		     
		     STORE SPACE(50) TO WTRANSPORTI,WMATRICULA_,WORIGEN,WDESTINO,WMATRICULA,WNOTAS,WCLIENTE,WFECHA_DEST

		     WFECHA_ORIG=CTOD("  -  -    ")
		     

		     SELECT 1
		     SCAN
		        
		     WPOSICION=POSICION
		     WPRECIO=PRECIO
		     WAGENCIA_PR=AGENCIA_PR
		     WAGENCIA_PO=AGENCIA_PO
		     WTRANSPORTI=TRANSPORTI
		     WMATRICULA_=MATRICULA_
		     WORIGEN=ORIGEN
		     WDESTINO=DESTINO
		     WMATRICULA=MATRICULA
		     
		     WNOTAS=NOTAS
		     WCLIENTE=CLIENTE
		     WFECHA_DEST=FECHA_DEST
		     WFECHA_ORIG=FECHA_ORIG
		    




*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

********   R E C O R D A R   P O S I B L E    E R R O R    D E    N U M E R A C I O N    D E    M E R E N D O L A S

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


		     
		     
		     
		     IF (MATRICULA="AGENCIA").AND.(MONTH(TTOD(FECHA_ORIG))=MES).and.(year(TTOD(fecha_orig))=anno)
		     CANNO=STR(ANNO,2,0)
		     
		     *IF (MATRICULA="AGENCIA").AND.(MONTH(TTOD(FECHA_ORIG))=MES).and.(SUBSTR(STR(year(TTOD(fecha_orig)),2,0),1,4)=SUBSTR(Canno,1,4))
		        *SET FILTER TO  year(TTOD(fecha_orig))=anno
		     
			     SELECT 2
			     
			     
			     APPEND BLANK
			     
			     IF ISNULL(WPRECIO)
			              WPRECIO=0
			       
			     ENDIF  
			     IF ISNULL(WAGENCIA_PO)
			              WAGENCIA_PO=0
			     ENDIF
			     
			     
			     
			     REPLACE POSICION WITH WPOSICION
			     REPLACE PRECIO WITH WPRECIO
			     REPLACE AGENCIA_PR WITH WAGENCIA_PR
			     REPLACE AGENCIA_PO WITH WAGENCIA_PO
			     REPLACE TRANSPORTI WITH WTRANSPORTI
			     REPLACE MATRICULA_ WITH WMATRICULA_
			     REPLACE ORIGEN WITH WORIGEN
			     REPLACE DESTINO WITH WDESTINO
			     REPLACE MATRICULA WITH WMATRICULA
			     REPLACE NOTAS WITH WNOTAS
			     
			     REPLACE CLIENTE WITH WCLIENTE
			     REPLACE FECHA_DEST WITH WFECHA_DEST
			     REPLACE FECHA_ORIG WITH WFECHA_ORIG
			          
			    
			     
			     SELECT 1
		     
		       *SET FILTER TO 
		     
		     ENDIF
		     
		     ENDSCAN
		     
		     
		     CLOSE DATABASES
		     DELETE FILE TABLA2.DBF
		   
		   
		   
		   
		   
		  
		     
		   
		   
		   ENDIF 
		    
		     *ON ERROR 
		    
		     *RUN DEL TABLA2.DBF
		     NUM=NUM+1
		     
		     IF num>999
		        CTABLA="[MMERENDOLA "+str(NUM,4)+"]"
		     
		      else
		        CTABLA="[MERENDOLA "+STR(NUM,3)+"]"
		     endif
		     
		     
		     ?CTABLA
		  
		ENDDO
		SQLDISCONNECT(0)

		*?CTABLA
		     
		*********************************************
		**********************************************
		************ SALIDA DE RESULTADOS
		****************************************
		*****************************************
		**********************************     


		wrespuesta="i"



		clear

		@ 5,5 say "Salida por (p)antalla o (i)mpresora  " get wrespuesta
		READ
		wrespuesta=LOWER(wrespuesta)


		DO CASE

		*************************************
		    CASE WRESPUESTA="i"
		*****************************************
		***************************************
		********************************************




		*USE C:\VISPARTES\NUEVO
		USE TABLA1 INDEX INTEMP
        *SET FILTER TO  year(TTOD(fecha_orig))=anno
		*INDEX ON CLIENTE+STR(YEAR(FECHA_ORIG),4)+STR(MONTH(FECHA_ORIG),2)+STR(DAY(FECHA_ORIG),2) TO INTEMP
		sET TALK OFF
		*mes=1
		clear

		*@ 1,2 say "introducir mes" get mes
		@ 2,1 say "SELECCIONAR FORMATO ADECUADO (A3 TRANSVERSAL)"
		read


		set print on prompt
		set device to print

		SET PRINTER FONT "Monospac821 BT",10


		@ 1,1 say "     "+cmonth(ctod("01-"+str(mes,2)+"-"+STR(ANNO,4)))+"   "+STR(ANNO,4)+"  ---------   A G E N C I A"


		@ 3,1 say "        REF  FECHA  CLIENTE    ORIGEN    DESTINO    TRANSPORTIST    MATRICULA   P. VENTA    P. COMPRA    DIFERENCIA "

		@ 4,1 say "        ______________________________________________________________________________________________________________"
		A=0
		B=0
		N=0
		GO TOP
		DO WHILE .NOT. EOF()


		       *IF (YEAR(FECHA_ORIG)=2006).AND.(MONTH(FECHA_ORIG)=mes).AND.(MATRICULA="AGENCIA")
		           DIFERENCIA=PRECIO-AGENCIA_PO

		           @ prow()+2,1 say str(int(agencia_pr),10)
		           @ prow(),12 say DTOC(fecha_orig)
		           @ prow(),24 say left(cliente,10)
		           @ prow(),36 say left(origen,10)
		           @ prow(),48 say left(destino,10)
		           @ prow(),60 say left(transporti,10)
		           @ prow(),72 say left(matricula_,10)
		           @ prow(),84 say str(agencia_po,10,2)
		           @ prow(),96 say str(precio,10,2)
		           @ prow(),108 say str(diferencia,10,2)
		           A=A+PRECIO
		           B=B+AGENCIA_PO
		           N=N+1

		        *ENDIF
		        
		        SKIP

		        if prow()+1>60
		             eject
		        endif
		ENDDO


		TDIFERENCIA=A-B

		if prow()+1>48
		     eject
		endif

		@ prow()+3,1 say "      PRECIO COMPRA= "+" "+STR(A,10,2)

		@ prow()+1,1 say "      PRECIO VENTA = "+" "+STR(B,10,2)

		@ prow()+1,1 say "      DIFERENCIA   = "+" "+STR(TDIFERENCIA,10,2)

		@ prow()+1,1 say "      NUMERO VIAJ. = "+" "+str(N)
		PORCENTAJE=TDIFERENCIA*100/A

		CLOSE INDEX
		CLOSE DATABASES

		set print off
		set printer to
		set device to screen

		CLEAR

		*********************************
		*********************************
		CASE wrespuesta="p"
		*********************************
		*********************************





		*USE C:\VISPARTES\NUEVO
		USE TABLA1 INDEX INTEMP
        *SET FILTER TO  year(TTOD(fecha_orig))=anno
		sET TALK OFF
		*INDEX ON CLIENTE+STR(YEAR(FECHA_ORIG),4)+STR(MONTH(FECHA_ORIG),2)+STR(DAY(FECHA_ORIG),2) TO INTEMP

		*mes=1
		clear

		*@ 1,2 say "Introducir mes" get mes
		*read
		CLEAR


		DELETE FILE TAGENCIA.TXT
		set device to FILE tagencia.txt





		@ 1,1 say "     "+cmonth(ctod("01-"+str(mes,2)+"-"+STR(ANNO,4)))+"   "+STR(ANNO,4)+ " ---------   A G E N C I A"
		@ 3,1 say "        REF  FECHA  CLIENTE    ORIGEN    DESTINO    TRANSPORTIST    MATRICULA   P. VENTA    P. COMPRA    DIFERENCIA "

		@ 4,1 say "        ______________________________________________________________________________________________________________"
		A=0
		B=0
		N=0
		GO TOP
		DO WHILE .NOT. EOF()
		       *IF (YEAR(FECHA_ORIG)=2006).AND.(MONTH(FECHA_ORIG)=mes).AND.(MATRICULA="AGENCIA")
		           DIFERENCIA=PRECIO-AGENCIA_PO
		           @ prow()+2,1 say str(int(agencia_pr),10)
		           @ prow(),12 say DTOC(fecha_orig)
		           @ prow(),24 say left(cliente,10)
		           @ prow(),36 say left(origen,10)
		           @ prow(),48 say left(destino,10)
		           @ prow(),60 say left(transporti,10)
		           @ prow(),72 say left(matricula_,10)
		           @ prow(),84 say str(agencia_po,10,2)
		           @ prow(),96 say str(precio,10,2)
		           @ prow(),108 say str(diferencia,10,2)
		           A=A+PRECIO
		           B=B+AGENCIA_PO
		           N=N+1

		        *ENDIF
		        
		        SKIP


		ENDDO


		TDIFERENCIA=A-B



		@ prow()+3,1 say "      PRECIO COMPRA= "+" "+STR(A,10,2)
		@ prow()+1,1 say "      PRECIO VENTA = "+" "+STR(B,10,2)
		@ prow()+1,1 say "      DIFERENCIA   = "+" "+STR(TDIFERENCIA,10,2)
		@ prow()+1,1 say "      NUMERO VIAJ. = "+" "+str(N)
		PORCENTAJE=TDIFERENCIA*100/A
		CLOSE INDEX
		CLOSE DATABASES
		set device to screen
		MODIFY FILE tagencia.txt noedit
		*DELETE FILE TAGENCIA.TXT

		clear


		********************************
		ENDCASE 
		********************************








RETURN

************************************************************
*********************************
******************************************************



PROCEDURE MODIFAC
STORE SPACE(10) TO WNUMFAC

CLEAR
@ 1,1 SAY "INTRODUCIR EL NUMERO DE FACTURA"
@ 2,1 SAY "ESPERAR A QUE APAREZCA LA PANTALLA CON LOS DATOS"
@ 3,1 SAY "PARA AÑADIR UN REGISTRO PULSAR CTRL+Y"
@ 4,1 SAY "COPIAR SIEMPRE EL CAMPO NUMFAC Y EL CAMPO CLIENTE"
@ 6,1 SAY "PARA BORRAR UN REGISTRO, PULSAR CON EL RATON EN EL RECUADRO PEQUEÑO"
@ 7,1 SAY "MAS A LA IZQUIERDA DEL REGISTRO"
@ 8,1 SAY "ESTE SE PONDRA DE COLOR NEGRO, SI SE PULSA OTRA VEZ, SE ANULA EL BORRADO"

@ 20,1 SAY "INTRODUCE NUMERO DE FACTURA: " GET WNUMFAC
READ


WNUMFAC=TRIM(WNUMFAC)
*SET FILTER TO NUMFAC=WNUMFAC
*DO form formulariomodificarfactura
*SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY NUMFAC,FECHA INTO CURSOR CURSOR1 READWRITE
USE CONCEPTO INDEX INFACFEC,INNUMERO,IN1REF
SET FILTER TO NUMFAC=WNUMFAC
*DO FORM MODIFAC
BROWSE
*SET EXCLUSIVE ON

*REINDEX
*SET EXCLUSIVE OFF
CLOSE INDEXES
CLOSE DATABASES
RETURN


********************************************************
*******************************
*********************************************************
*IMPRESION DE FACTURA DE VARIAS PAGINAS MEDIANTE EL DISEÑADOR DE INFORMES
PROCEDURE FNACREP
STORE SPACE(10) TO WNUMFAC
STORE SPACE(10) TO WCLIENTE
WRESP="PA"
CLEAR

@ 1,1 SAY "INTRODUCE NUMERO DE FACTURA: " GET WNUMFAC
@ 2,1 say "IMPRESION EN (P)APEL/PD(F)/S(M)ET/PE(T)RA/(PA)TINTER:  " GET WRESP 
READ
WRESP=LOWER(WRESP)

SET EXCLUSIVE ON

USE FACTURAS INDEX INFAC,INCLIFE


REINDEX

SET EXCLUSIVE OFF
WNUMFAC=TRIM(WNUMFAC)
SEEK WNUMFAC

IF FOUND()

   *EDIT
ENDIF

WCLIENTE=TRIM(CLIENTE)
WCLIENTE=TRIM(WCLIENTE)
CLOSE INDEXES 
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off

*IF WRESP="p"
DO CASE
  CASE WRESP="p"
   REPORT FORM C:\visprogfac2\informefacturasEXTRANJERO.frx TO PRINTER PROMPT PREVIEW
  CASE WRESP="m"
   REPORT FORM C:\visprogfac2\informefacturasMET.frx TO PRINTER PROMPT PREVIEW
  CASE WRESP="t"
   REPORT FORM C:\visprogfac2\informefacturaspetra.frx TO PRINTER PROMPT PREVIEW
  CASE WRESP=="pa"
REPORT FORM C:\visprogfac2\informefacturaspetra.frx TO PRINTER PROMPT PREVIEW  OTHERWISE
   REPORT FORM C:\visprogfac2\informefacturaspatinter2.frx TO PRINTER PROMPT PREVIEW
ENDCASE

CLOSE DATABASES 
SET CENTURY on
RETURN



***********************************************************
*************************************************************
***************************************************************
** VIAJES DE PROVEEDORES
***********************************************
************************************




PROCEDURE VIAJPROV

STORE SPACE(10) TO WPROVEEDOR
WMES=0
WANNO=0
CLEAR

@ 5,5 SAY "INTRODUCE EL PROVEEDOR:     " GET WPROVEEDOR
@ 6,5 SAY "MES EN NUMERO:              " GET WMES
@ 7,5 SAY "AÑO:                        " GET WANNO
READ






SELECT * FROM CONCEPTO WHERE TRANSPORT=WPROVEEDOR.AND.MONTH(FECHA)=WMES.AND.YEAR(FECHA)=WANNO ORDER BY FECHA INTO CURSOR CURSOR1
 
REPORT FORM CONCEPTOS_PROVEEDORES TO PRINTER PROMPT PREVIEW




CLOSE DATABASES 

RETURN

************************
************************
**************************


PROCEDURE agenciacabrero





CLOSE INDEXES
CLOSE DATABASES

USE FACTURAS INDEX INFAC,INCLIFE
SEEK WNUMFAC
WCLIENTE=TRIM(CLIENTE)


CLOSE INDEXES
CLOSE DATABASES

SELECT * FROM c:\visprogfac2\clientes WHERE CLAVE=WCLIENTE INTO CURSOR CURSORCLIENTES
SELECT * FROM C:\VISPROGFAC2\FACTURAS WHERE FACTURA=WNUMFAC INTO CURSOR CURSORFACTURA
SELECT * FROM C:\VISPROGFAC2\CONCEPTO WHERE NUMFAC=WNUMFAC ORDER BY FECHA INTO CURSOR CURSORCONCEPTO
SET CENTURY off
REPORT FORM C:\visprogfac2\agenciacabrero2.frx TO PRINTER PROMPT PREVIEW
CLOSE DATABASES 
SET CENTURY on




RETURN




*************************+
***********************
*****************************



PROCEDURE facturacion

CLOSE INDEXES 
CLOSE DATABASES 
_screen.FontSize=15
_screen.FontName="COURIER NEW"


SET DATE ITALIAN
SET CENTURY on

fechaini=CTOD("  -  -    ")
fechafin=CTOD("  -  -    ")
nifcliente="000000000"

CLEAR
@ 5,5 say "fecha inicial" get fechaini
@ 7,5 say "fecha final"   get fechafin
@ 9,5 say "nif cliente (000000000 si todos)" get nifcliente
READ


IF nifcliente="000000000"

SELECT * FROM facturas JOIN clientes ON cliente=clave WHERE fecha>=fechaini.and.fecha<=fechafin INTO CURSOR cursor1

ELSE

SELECT * FROM facturas JOIN clientes ON cliente=clave WHERE fecha>=fechaini.and.fecha<=fechafin INTO CURSOR cursor2

SELECT * FROM cursor2 WHERE nif=nifcliente INTO CURSOR cursor1

endif

REPORT FORM anuales TO PRINTER PROMPT preview


CLOSE INDEXES 
CLOSE DATABASES 



return
