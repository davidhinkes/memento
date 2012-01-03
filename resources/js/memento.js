function uploadFile(f) {
  reader = new FileReader();
  reader.onload = function(e) {
    var fd = e.target.result;
    $.ajax({
      type: 'POST',
      url: '/upload',
      contentType: 'multipart/form-data',
      data: { data:fd },
      success: function(e) { alert('done!'); },
      timeout: (10*60*1000)
    });
  }
  reader.readAsDataURL(f);
}

function handleSubmit(evt) {
for (var i = 0, f; f = _files[i]; ++i) {
  uploadFile(f.file);
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
$("#uploader").pluploadQueue({
  runtimes: 'html5',
  url : 'upload',
  // Don't use chuncked option.  Server does not support it.
  max_file_size : '100mb',
  unique_names : true,
});
//$('#files').bind('change', handleFileSelect );
//$('#submit').bind('click', handleSubmit );
});

