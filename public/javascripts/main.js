function deleteDialog(e) {
    var r = confirm("Confirmer la suppression");
    if (r) {
        return true;
    } else {
        e.preventDefault();
    }
}

function editeur() {
    tinymce.init({
        selector: '#mytextarea',
        plugins: [
            'advlist autolink link image imagetools lists charmap print preview hr anchor pagebreak spellchecker',
            'searchreplace wordcount visualblocks visualchars code fullscreen insertdatetime media nonbreaking',
            'save table contextmenu directionality emoticons template paste textcolor'
        ],
        images_upload_url: '/uploadimage',
        automatic_uploads: true
    });
}

function afficheCarte(champ, fichier, depart, arrivee) {
    var mymap = L.map(champ);
    var hikebikemapUrl = 'http://{s}.tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png';
    var hikebikemapAttribution = 'Map Data © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors';
    var hikebikemap = new L.TileLayer(hikebikemapUrl, {maxZoom: 17, attribution: hikebikemapAttribution});
    hikebikemap.addTo(mymap);
    var customLayer = L.geoJson(null, {
        // http://leafletjs.com/reference.html#geojson-style
        style: function(feature) {
            return { color: '#f00' };
        }
    });
    var departIcon = L.icon({
        iconUrl: '/assets/images/departS.png',
        iconSize:     [40, 40],
        iconAnchor:   [15, 37]
    });
    var arriveeIcon = L.icon({
        iconUrl: '/assets/images/arriveeS.png',
        iconSize:     [40, 40],
        iconAnchor:   [36, 39]
    });
    var runLayer = omnivore.gpx(fichier, null, customLayer)
.on('ready', function() {
        mymap.fitBounds(runLayer.getBounds());
    })
        .on('click', function() {
            $('#myModal').modal('toggle'); })
        .addTo(mymap);
    L.marker([depart.split(",")[0], depart.split(",")[1]], {icon: departIcon})
.addTo(mymap);
    L.marker([arrivee.split(",")[0], arrivee.split(",")[1]], {icon: arriveeIcon})
.addTo(mymap);
}