(******************************************************
*
*   Programa para la gestion de un pabellon deportivo
*   Practica 3
*   Autor: Miguel Angel Cabrero Arranz
*   DNI: 12374764M
*******************************************************)






MODULE Pabellon;
       FROM InOut IMPORT Write, Read, WriteString, WriteInt, WriteLn, ReadInt,ReadString;
       FROM GesCal IMPORT MostrarCalendario,adias;
       TYPE
          (*adias=ARRAY [1..50] OF INTEGER; *)
          tipocadena=ARRAY [0..10] OF CHAR;
          cfecha=ARRAY [0..2] OF CHAR;
          tipoevento= RECORD
                         codigo:INTEGER;
                         equipo1,equipo2:tipocadena;
                         dia:INTEGER;
                         mes:INTEGER;
                         anyo:INTEGER;
                         hora:INTEGER;
                         minuto:INTEGER;
                         tipoevento:tipocadena;
                         vacio:BOOLEAN;
                      END;
          tipopabellon=ARRAY [1..50] OF tipoevento;

      VAR
         pabellon1:tipopabellon;
         opcion:CHAR;
         n,rvacio,j:INTEGER;
         wcodigo:INTEGER;
         wdia:INTEGER;
         wmes:INTEGER;
         wanyo:INTEGER;
         rencontrado,cencontrado,fencontrado,eencontrado,testigo:BOOLEAN;
         wevento:tipocadena;
         cdia,cmes:cfecha;
         dias:adias;







   BEGIN


        (* INICIALIZAR ARRAY *)
        FOR n:=1 TO 50 DO

             pabellon1[n].vacio:=TRUE;

        END;

        opcion:=" ";

        REPEAT
              WriteLn;
              WriteString("   GESTION DE EVENTOS DEPORTIVOS");WriteLn;
              WriteString("   =============================");WriteLn;
              WriteString("Alta de evento deportivo                 (Pulsar A)");WriteLn;
              WriteString("Eliminar evento deportivo                (Pulsar E)");WriteLn;
              WriteString("Listado de eventos deportivos            (Pulsar L)");WriteLn;
              WriteString("Mostrar calendario de eventos deportivos (Pulsar M)");WriteLn;
              WriteString("Salir                                    (Pulsar S)");WriteLn;
              WriteString("   =============================");WriteLn;
              WriteString("INTRODUZCA UNA OPCION VALIDA (A|E|L|M|S)");WriteLn;
              Read(opcion);opcion:=CAP(opcion);Write(opcion);WriteLn;
              CASE opcion OF
                   "A":
                        (*COMPROBACION DE CODIGO Y FECHA*)
                        WriteString("Codigo del evento:");ReadInt(wcodigo);WriteLn;

                        cencontrado:=FALSE;
                        FOR n:=1 TO 50 DO
                            IF pabellon1[n].codigo=wcodigo THEN
                                cencontrado:=TRUE;
                            END;
                        END;
                        IF cencontrado THEN
                                 WriteLn;WriteString("El codigo ya existe");WriteLn;
                        ELSE
                             WriteString("Dia del evento:");ReadInt(wdia);WriteLn;
                             WriteString("Mes del evento:");ReadInt(wmes);WriteLn;
                             WriteString("Año del evento:");ReadInt(wanyo);WriteLn;
                             fencontrado:=FALSE;
                             FOR n:=1 TO 50 DO
                               IF (pabellon1[n].dia=wdia) AND (pabellon1[n].mes=wmes) AND (pabellon1[n].anyo=wanyo)  THEN
                                 fencontrado:=TRUE;
                               END;
                             END;
                             IF fencontrado THEN
                                 WriteString("Ya existe un evento en esa fecha");WriteLn;
                             ELSE
                             (* INRODUCCION DE DATOS *)


                            (* ENCONTRAR EL PRIMER REGISTRO VACIO *)
                              rencontrado:=FALSE;
                            FOR n:=1 TO 50 DO
                              IF pabellon1[n].vacio=TRUE THEN
                                 rencontrado:=TRUE;
                                 rvacio:=n;
                              END;
                            END;

                            pabellon1[rvacio].codigo:=wcodigo;

                            WriteString("Nombre del primer equipo:");ReadString(pabellon1[rvacio].equipo1);WriteLn;
                            WriteString("Nombre del segundo equipo:");ReadString(pabellon1[rvacio].equipo2);WriteLn;
                            pabellon1[rvacio].dia:=wdia;
                            pabellon1[rvacio].mes:=wmes;
                            pabellon1[rvacio].anyo:=wanyo;
                            WriteString("Hora del evento:");ReadInt(pabellon1[rvacio].hora);Write(":");
                            ReadInt(pabellon1[rvacio].minuto);WriteLn;
                            WriteString("Tipo de evento:");ReadString(pabellon1[rvacio].tipoevento);WriteLn;
                            pabellon1[rvacio].vacio:=FALSE;
                            END;
                        END; |




                   "E":
                        (* ELIMINAR EVENTO DEPORTIVO *)
                         WriteString("  ELIMINAR EVENTO DEPORTIVO");WriteLn;
                         WriteString("=============================");WriteLn;

                         WriteString("Codigo del evento a eliminar:");ReadInt(wcodigo);WriteLn;
                         cencontrado:=FALSE;
                        FOR n:=1 TO 50 DO
                            IF pabellon1[n].codigo=wcodigo THEN
                                cencontrado:=TRUE;
                                pabellon1[n].vacio:=TRUE;
                                pabellon1[n].codigo:=0;
                                pabellon1[n].dia:=0;
                                pabellon1[n].mes:=0;
                                pabellon1[n].anyo:=0;
                                WriteString("Se ha eliminado el evento");WriteLn;
                            END;
                        END;
                        IF cencontrado=FALSE THEN
                               WriteString("No se encuentra el codigo introducido");WriteLn;
                               WriteString("No se ha dado de baja ningun evento");
                        END;  |
                   "L":
                        (* LISTADO DE EVENTOS DEPORTIVOS *)
                         WriteString("     LISTADO DE EVENTOS DEPORTIVOS");WriteLn;
                         WriteString("     =============================");WriteLn;
                         WriteString("Tipo de evento deportivo:");ReadString(wevento);WriteLn;
                         eencontrado:=FALSE;
                         FOR n:=1 TO 50 DO
                              testigo:=TRUE;
                              FOR j:=0 TO 10 DO
                                    IF pabellon1[n].tipoevento[j]<>wevento[j] THEN
                                                 testigo:=FALSE;
                                    END;
                              END;

                            IF testigo THEN
                                eencontrado:=TRUE;
                            END;
                         END;
                         IF eencontrado THEN
                            WriteString("    LISTADO DE EVENTOS DEPORTIVOS");WriteLn;
                            WriteString("    =============================");WriteLn;
                            FOR n:=1 TO 50 DO
                                   testigo:=TRUE;
                                   FOR j:=0 TO 10 DO
                                       IF pabellon1[n].tipoevento[j]<>wevento[j] THEN
                                                 testigo:=FALSE;
                                       END;
                                   END;



                                  IF testigo THEN
                                      WriteString(pabellon1[n].tipoevento);WriteString(". ");WriteString(pabellon1[n].equipo1);WriteString(" vs ");
                                      WriteString(pabellon1[n].equipo2);WriteString(". ");
                                         IF pabellon1[n].dia<10 THEN
                                            cdia[0]:="0";
                                            CASE pabellon1[n].dia OF
                                               1: cdia[1]:="1"|
                                               2: cdia[1]:="2"|
                                               3: cdia[1]:="3"|
                                               4: cdia[1]:="4"|
                                               5: cdia[1]:="5"|
                                               6: cdia[1]:="6"|
                                               7: cdia[1]:="7"|
                                               8: cdia[1]:="8"|
                                               9: cdia[1]:="9"|
                                               ELSE
                                            END;

                                            WriteString(cdia);
                                         ELSE

                                            WriteInt(pabellon1[n].dia,2);
                                         END;
                                      Write("-");
                                      IF pabellon1[n].mes<10 THEN
                                            cmes[0]:="0";
                                             CASE pabellon1[n].mes OF
                                               1: cmes[1]:="1"|
                                               2: cmes[1]:="2"|
                                               3: cmes[1]:="3"|
                                               4: cmes[1]:="4"|
                                               5: cmes[1]:="5"|
                                               6: cmes[1]:="6"|
                                               7: cmes[1]:="7"|
                                               8: cmes[1]:="8"|
                                               9: cmes[1]:="9"|
                                               ELSE
                                            END;



                                            WriteString(cmes);
                                         ELSE

                                            WriteInt(pabellon1[n].mes,2);
                                      END;

                                      Write("-");WriteInt(pabellon1[n].anyo,4);WriteString(". ");
                                      WriteInt(pabellon1[n].hora,2);Write(":");WriteInt(pabellon1[n].minuto,2);WriteString(" horas");WriteLn;WriteLn;

                                  END;
                             END;
                          ELSE
                            WriteString("No existen eventos de este tipo");WriteLn;
                         END;  |



                   "M":
                         (* MOSTRAR CALENDARIO DE EVENTOS *)
                         wmes:=0;
                         wanyo:=0;
                         WriteString("     MOSTRAR CALENDARIO EVENTOS");WriteLn;
                         WriteString("=====================================");WriteLn;
                         WriteString("Mes:");ReadInt(wmes);WriteLn;
                         WriteString("Año:");ReadInt(wanyo);WriteLn;
                          FOR n:=1 TO 50 DO
                             dias[n]:=0;
                          END;

                          fencontrado:=FALSE;
                             FOR n:=1 TO 50 DO
                               IF (pabellon1[n].mes=wmes) AND (pabellon1[n].anyo=wanyo)  THEN
                                 fencontrado:=TRUE;
                                 dias[n]:=pabellon1[n].dia;
                               END;
                             END;

                         IF fencontrado=FALSE THEN
                             WriteString("No existe ningun evento programado en esa fecha");
                             WriteLn;WriteLn;

                             MostrarCalendario(wmes,wanyo,dias);

                         ELSE


                         MostrarCalendario(wmes,wanyo,dias);


                         WriteLn;WriteLn;
                         FOR n:=1 TO 50 DO
                            IF (pabellon1[n].mes=wmes) AND (pabellon1[n].anyo=wanyo) THEN
                               IF pabellon1[n].dia<10 THEN
                                            cdia[0]:="0";
                                            CASE pabellon1[n].dia OF
                                               1: cdia[1]:="1"|
                                               2: cdia[1]:="2"|
                                               3: cdia[1]:="3"|
                                               4: cdia[1]:="4"|
                                               5: cdia[1]:="5"|
                                               6: cdia[1]:="6"|
                                               7: cdia[1]:="7"|
                                               8: cdia[1]:="8"|
                                               9: cdia[1]:="9"|
                                               ELSE
                                            END;

                                            WriteString(cdia);
                                         ELSE

                                            WriteInt(pabellon1[n].dia,2);
                                         END;
                                      Write("-");
                                      IF pabellon1[n].mes<10 THEN
                                            cmes[0]:="0";
                                             CASE pabellon1[n].mes OF
                                               1: cmes[1]:="1"|
                                               2: cmes[1]:="2"|
                                               3: cmes[1]:="3"|
                                               4: cmes[1]:="4"|
                                               5: cmes[1]:="5"|
                                               6: cmes[1]:="6"|
                                               7: cmes[1]:="7"|
                                               8: cmes[1]:="8"|
                                               9: cmes[1]:="9"|
                                               ELSE
                                            END;



                                            WriteString(cmes);
                                         ELSE

                                            WriteInt(pabellon1[n].mes,2);
                                      END;


                              Write("-");WriteInt(pabellon1[n].anyo,4);
                              WriteString(" ");WriteString(pabellon1[n].equipo1);WriteString(" - ");
                              WriteString(pabellon1[n].equipo2);WriteString(". ");WriteString(pabellon1[n].tipoevento);
                              ;WriteString(". ");
                              WriteInt(pabellon1[n].hora,2);Write(":");WriteInt(pabellon1[n].minuto,2);WriteString(" horas");WriteLn;
                            END;
                         END;
                       END;  |
                   ELSE
              END;
        UNTIL opcion = "S";
  END Pabellon.
