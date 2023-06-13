function pid=sistema_experto(pid,num,den,espec)
% Numero de vuelta
  k = 1;
% Simulamos el modelo
  [tout,yout]=simular(pid,num,den);
    
% Calculo de las caracteristicas del sistema
  [tr,tp,Mp,ts,ys]=caracteristicas(tout,yout);
  [tout,yout]=simular(pid,num,den,tr,tp,Mp,ts,ys);
  
% Abrimos el modelo Simulink
  open_system('modelo');
  disp(' ');
  disp(' Pulse enter para ejecutar el control experto');
  pause;
  
% Incrementar o decrementar las especificaciones
  if tr<=espec(1), incrementar_tr=1; else incrementar_tr=0; end
  if tp<=espec(2), incrementar_tp=1; else incrementar_tp=0; end   
  if Mp<=espec(3), incrementar_Mp=1; else incrementar_Mp=0; end   
  if ts<=espec(4), incrementar_ts=1; else incrementar_ts=0; end   
  if ys<=espec(5), incrementar_ys=1; else incrementar_ys=0; end   
  
% Reglas del sistema experto para adaptar las caracteristicas a las especificaciones
  salir=0;
  while ~salir
        
        % Regla para el tiempo de subida
        if espec(1)<tr % tiempo de subida demasiado largo -> aumentar accion proporcional
            pid(1)=pid(1)+0.5;
        else
            if espec(1)>(tr+0.15) % caso tiempo de subida demasiado pequeno
                pid(1)=pid(1)-0.5; % reducir accion proporcional
            end
        end

        % Regla para el estado estacionario
        if abs(espec(5) - ys) > 0.01
            % es el caso de un error estacionario, es decir la accion
            % integral no es suficiente, aumentamos la Ki
            pid(2)=pid(2)+0.3;
        end 
      
        % Regla para la sobrelongación
        if espec(3)<Mp
            % la sobrelongacion es demasiado grande, es decir que la accion
            % derivativa no es suficiente, entonces aumentamos Kd
            pid(3)=pid(3)+3.4;
        else
            % si la sobrelongación es menor que Mp es que la accion
            % derivativa funciona bien, disminuimos la Kd
            pid(3)=pid(3)-0.5;
        end

      
      % Caracteristicas del sistema bajo la nueva situacion
        [tout,yout]=simular(pid,num,den,tr,tp,Mp,ts,ys);
        [tr,tp,Mp,ts,ys]=caracteristicas(tout,yout);
        
      % Si se cumplen las especificaciones, entonces salir
      
         if tr<=espec(1) && Mp<=espec(3) && ys<=espec(5)
             salir=1;
         else
             disp("Vuelta n°" + k)
             disp("    Con " + "Tr = " + tr + ", Mp = " + Mp + ", ys = " + ys)
             disp("    PID: Kp = " + pid(1) + ", Ki = " + pid(2) + ", Kd = " + pid(3))
         end
         k = k + 1;

        %if incrementar_tr
            %if tr>espec(1)
                %salir=1;
            %end
        %else
            %if tr<=espec(1)
                %salir=1;
            %end
        %end
  end
  [tout,yout]=simular(pid,num,den,tr,tp,Mp,ts,ys);
  disp(' ');
  disp(' PID sintonizado, pulse enter para salir');
  pause;
  
% Cerramos el modelo Simulink
  close_system('modelo');