function handleSubmit(evt) {
for (var i = 0, f; f = _files[i]; ++i) {
  alert(f.file.name);
}
}
function handleFileSelect(evt) {
var files = evt.target.files;
var shoot = $('#shoot').val();
for (var i = 0, f; f = files[i]; i++) {
  var k = { "file": f, "shoot": shoot };
  _files.push(k);
}
for (var i = 0, f; f = _files[i]; ++i) {
  $('#preview').append('<div><p>' + f.file.name + '</p><p>' + f.shoot + '</p></div>');
}
}
$(document).ready(function(){
_files = [];
$('#files').bind('change', handleFileSelect );
$('form').bind('submit', handleSubmit );
});

