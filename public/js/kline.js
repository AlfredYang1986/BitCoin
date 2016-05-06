/**
 * SOSOBTCæ’ä»¶ï¼ŒCreate By Ymjã€Li.hq 
 */
(function(window, undefined){
	var __hasProp = {}.hasOwnProperty;
	
	function Model(modelName){
		var _fn = arguments.callee;
		this.modelName = modelName;
		//æŽ¥å—å‚æ•°å¹¶åˆå§‹åŒ–
		this.init 	= (function(modelName){
			if (modelName != undefined && _fn.init[modelName]) return _fn.init[modelName];
			return function(){};
		})(modelName);
	}
	//æš‚ä¸æ”¯æŒæ·±åº¦å¤åˆ¶
	Model.extend = function(){
		var result = arguments[0] || {};
		for ( var i = 1; i < arguments.length; i++) {
			for (var key in arguments[i]){
				result[key] = arguments[i][key];
			}
		}
		return result;
	};
	//æ¨¡å—çš„å…¬ç”¨æ–¹æ³•
	Model.extend(Model.prototype, {
		extend : Model.extend,
		/**
		 * ç”Ÿæˆiframe
		 */
		makeIframe : function(opt){
			var iframe = document.createElement('iframe'),
				element = document.getElementById(opt.id);
			
			iframe.src = this.src;
			iframe.setAttribute("frameborder", "0");
			iframe.setAttribute("border", "0");
			iframe.setAttribute("width", "100%");
			iframe.setAttribute("height", "100%");
			iframe.setAttribute("id", "sosobtc_market_iframe");
			if (typeof opt.completeLoad == "function"){
				iframe.onload = opt.completeLoad;
			}
			//iframeæ’å…¥æ–‡æ¡£
			element.appendChild(iframe);
		}
	});
	//æ¨¡å—åˆ—è¡¨
	Model.init = {
		//å®žæ—¶ä»·æ ¼
		Market	: function(object){
			var opt = this.extend({
				//é»˜è®¤å‚æ•°
				row  		: ['price', 'buy', 'sell', 'high', 'low', 'vol'],
				short_tip 	: true,
				show_tip: true,
				completeLoad: function(){}
			}, (object || {}));
			//é»˜è®¤å‚æ•°
			var param = [];
			if (opt.btc && opt.btc instanceof Array){
				param.push("btc=" + encodeURIComponent(opt.btc.join("|")));
			}
			if (opt.ltc && opt.ltc instanceof Array){
				param.push("ltc=" + encodeURIComponent(opt.ltc.join("|")));
			}
			if (opt.alt && opt.alt instanceof Array){
				param.push("alt=" + encodeURIComponent(opt.alt.join("|")));
			}
			if (opt.bter && opt.bter instanceof Array){
				param.push("bter=" + encodeURIComponent(opt.bter.join("|")));
			}
			if (opt.others && opt.others instanceof Array){
				param.push("others=" + encodeURIComponent(opt.others.join("|")));
			}
			console.log(param);
			param.push("short_tip=" + ((opt.short_tip) ? "1" : "0"));
			param.push("show_tip=" + ((opt.show_tip) ? "1" : "0"));
			param.push("td_height=" + ((opt.td_height) ? opt.td_height : ""));
			//æ˜¾ç¤ºé¡¹
			if (opt.row && opt.row instanceof Array){
				param.push("row=" + encodeURIComponent(opt.row.join("|")));
			}
			this.src = "http://www.sosobtc.com/plugin/market" + (param.length > 0 ? "?" + param.join("&") : "");
			this.makeIframe(opt);
		},
		//Kçº¿å›¾
		Kline	: function(object){
			var opt = this.extend({
				//é»˜è®¤å‚æ•°
				sid: "",					// 
				
				enableChatRoom: false,		// æ˜¯å¦ä½¿ç”¨èŠå¤©å®¤
				chatRoomHeight: 180,		// èŠå¤©å®¤é«˜åº¦
				chatRoomWidth: 320,			// èŠå¤©å®¤å®½åº¦
				enableHotKeyEsc: true,		// æ˜¯å¦ä½¿ç”¨å¿«æ·é”®Escæ¥å…³é—­èŠå¤©å®¤
				enableHotKeyEnter: true,	// æ˜¯å¦ä½¿ç”¨å¿«æ·é”®Enteræ¥æ‰“å¼€èŠå¤©å®¤
				openChatRoom: true,			// æ˜¯å¦åœ¨é¡µé¢åŠ è½½å®Œæˆæ—¶æ‰“å¼€èŠå¤©å®¤
				completeLoad: function(){}
				
			}, (object || {}));
			
			this.src = "http://www.sosobtc.com/plugin/kline?" + _ob2uri(opt);
			this.makeIframe(opt);
			
			function _ob2uri(ob, encode){
				encode = typeof encode === "undefined" ? true : Boolean(encode);
				
				var ret = '';
				for(var prop in ob){
					if(!__hasProp.call(ob, prop))
						continue;
					ret += prop + '=' + _string(ob[prop]) + '&'
				}
				if(!!ret){
					ret = ret.slice(0, -1);
				}
				return ret;
				
				function _string(ob){
					return encode ? encodeURIComponent(String(ob)) : String(ob);
				}
			}
			
			function _extend(){
				
			}
		}
	};
	function sosobtc(modelName){
		return new Model(modelName);
	}
	window.SOSOBTC = sosobtc;
})(window);