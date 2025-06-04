var _____WB$wombat$assign$function_____ = function(name) {return (self._wb_wombat && self._wb_wombat.local_init && self._wb_wombat.local_init(name)) || self[name]; };
if (!self.__WB_pmw) { self.__WB_pmw = function(obj) { this.__WB_source = obj; return this; } }
{
  let window = _____WB$wombat$assign$function_____("window");
  let self = _____WB$wombat$assign$function_____("self");
  let document = _____WB$wombat$assign$function_____("document");
  let location = _____WB$wombat$assign$function_____("location");
  let top = _____WB$wombat$assign$function_____("top");
  let parent = _____WB$wombat$assign$function_____("parent");
  let frames = _____WB$wombat$assign$function_____("frames");
  let opener = _____WB$wombat$assign$function_____("opener");

$(document).ready(function(){
	resolucao();	
	
});

$(window).resize(function(){ resolucao(); });

function resolucao(){
	if($(this).width() <= 1060){ $('html').addClass('_320'); }else{ $('html').removeClass('_320'); }
	if($(this).width() > 1060){    $('html').addClass('_1000');  }else{ $('html').removeClass('_1000'); }	
}

}
/*
     FILE ARCHIVED ON 03:57:53 May 31, 2021 AND RETRIEVED FROM THE
     INTERNET ARCHIVE ON 01:06:17 Jan 01, 2025.
     JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.

     ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
     SECTION 108(a)(3)).
*/
/*
playback timings (ms):
  captures_list: 0.554
  exclusion.robots: 0.018
  exclusion.robots.policy: 0.009
  esindex: 0.01
  cdx.remote: 4.445
  LoadShardBlock: 215.925 (3)
  PetaboxLoader3.resolve: 182.979 (3)
  PetaboxLoader3.datanode: 119.173 (4)
  load_resource: 91.953
*/