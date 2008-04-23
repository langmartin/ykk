var data = {foo:1,bar:2,baz:[1,{foo:[1,2]},'3']};

function ajaxTest(e){
	var type = $(e.target).attr('id');
	var ctype = (type == 'json') ? "application/jsonrequest" : "text/xml";
	$.ajaxSetup({contentType: ctype});
	$.post("/ajax",
	       data,
	       function(r){ $('#' + type).html('<pre>' + r + '</pre>'); },
	       type);
}

// $(function(){ $('#json,#xml').click(ajaxTest); });

function xmlTest(){
	var xml = new XMLHttpRequest();
	xml.open('POST','/ajax',true);
	xml.onreadystatechange = function() {
		if (xml.readyState == 4) {
			$('#xml').html(xml.responseText);
		}
	}
	xml.send({foo:1,bar:2});
}
