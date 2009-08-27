<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html401/transitional.dtd">
<html>
    <head>
        <title>Genexpression Report</title>
    </head>
    <body>
<?php
// Verbindungsaufbau und Auswahl der Datenbank
$dbconn = pg_connect("host=localhost dbname=gepr user=postgres")
    or die('Verbindungsaufbau fehlgeschlagen: ' . pg_last_error());

// Eine SQL-Abfrge ausführen
$query = 'SELECT * FROM celfile';
$result = pg_query($query) or die('Abfrage fehlgeschlagen: ' . pg_last_error());

// Ergebnisse in HTML ausgeben
echo "<table border>\n";

// Überschriften
echo "<tr> <td>Nr.</td> <td>CEL-File</td><td>Name</td> <td>Vorname</td>";
echo "<td>Geburtsdatum</td> <td>Straße</td><td>Ort</td> <td>PLZ</td>";
echo "<td>Diagnose</td> <td>IG-Typ</td><td>Leichtkette</td> <td>Geschlecht</td></tr>";



while ($line = pg_fetch_array($result, null, PGSQL_ASSOC)) {
    echo "\t<tr>\n";
    foreach ($line as $col_value) {
        echo "\t\t<td>$col_value</td>\n";
    }
    echo "\t</tr>\n";
}
echo "</table>\n";

// Speicher freigeben
pg_free_result($result);

// Verbindung schließen
pg_close($dbconn);
?>

    </body>
</html>

