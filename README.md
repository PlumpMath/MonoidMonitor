# MonoidMonitor
Un acumulador de estadísticas (en línea, sin buffer) con DSL (reglas de agregación).
*Nota:* es sólo por diversión, no es algo productivo.

# Descripción
Permite acumular estadísticas sobre cualquier tipo de valores de entrada (típicamente eventos de algún tipo) y generar los acumulados (típicamente por horas, días, ...).
Habitualmente se usa para monitorizar sistemas (ej. el uso de disco, red, memoria, ... en el hardware de un servidor; el número de peticiones, fallos, tiempos de respuesta, ... en un servidor web; el número de llamadas de un pesado, el número de veces que se va al baño, ... en una oficina).

# Algunas características
1. el *core* ocupa menos de 50 líneas :P
2. funciona en línea, es decir, no requiere almacenar los eventos, actualiza directamente todos los contadores afectados (parecido a programación reactiva).
3. permite acumular cualquier contador intermedio (los finales pueden ser cualquier cosa) que sea un monoide conmutativo (abeliano).
4. aunque no está implementado, el esquema permite actualización retrasada de contadores dependientes (ej. un valor acumulado en horas, días, meses y años puede acumular sólo las horas, cada día al día, cada mes al mes, ...). Seguirá siendo en línea porque sólo requiere almacenar otro valor por contador (un check de "traspasado").
5. DSL de reglas de agregación de contadores

# Ejemplo de reglas de agregado de un "servidor web" para acumular estadísticas de página

    web_server_rules =
        [
         -- Hace disponible cualquier contador en cualquier tiempo t' > t
         ((Years /=) . timescaleType . timescale) &&. startWith "web.page." :=> (return . reduced)
         
         -- Podemos generar el tamaño medio
        , startWith "web.page.size" :=> (return . toAvgSum   "stat.page.avg.size")
        , startWith "web.page.view" :=> (return . toAvgCount "stat.page.avg.size")

         -- O la velocidad media en que se generan los datos (útil para saber si el cuello está en nosotros o en el hardware)
        , startWith "web.page.size" :=> (return . toAvgSum   "stat.page.avg.speedgen")
        , startWith "web.page.time" :=> (return . toAvgCount "stat.page.avg.speedgen")
        ]
