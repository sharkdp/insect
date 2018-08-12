if (typeof require === "function") bigInt = require("../insect.js");

//Before all:
var insectEnv = Insect.initialEnvironment;	//set environment variable at first

//send command to insect as string "line"
function insect(line) {
	// Skip empty lines or line comments
	var lineTrimmed = line.trim();
	if (lineTrimmed === "" || lineTrimmed[0] === "#") {
	return;
	}
	// Run insect
	var res = Insect.repl(Insect.fmtJqueryTerminal)(insectEnv)(line);
	insectEnv = res.newEnv;

	// Handle shell commands
	if (res.msgType == "clear") {
		// Clear screen:
		this.clear();
		return "";
	}
	else if (res.msgType == "quit") {
		// Treat as reset:
		this.clear();
		insectEnv = Insect.initialEnvironment;
		return "";
	}
	//return res.msg;	//return full response
	//console.log(res.msg);
	var result = res.msg.split("=")[1];		//response only after "="
	var result = result.replace(" [[;;;hl-value]", '').replace('] [[;;;hl-unit]',' ').replace(']', '');
	if(result.indexOf('[[;;;hl-unit]')!==-1){result = result.replace('[[;;;hl-unit]',' ').replace(']', '');}
	//console.log(result);
	
	return result;	//only value and unit
}

/*
//make response from values and units to compare this
function expected_response(value, unit, convert){
	//response if convert...

	//full response
	var response = '\n  [[;;;hl-value]'+value
		+'] [[;;;hl-unit]'+unit
		+']\n\n   = [[;;;hl-value]'+value
		+'] [[;;;hl-unit]'+unit
		+']'
	;

	//only after "="
	var response = '[[;;;hl-value]'+value
		+'] [[;;;hl-unit]'+unit
		+']'
	;
	//but this can be just a string to comare, so this code is commented.

	if(convert==='converted'){return response;}	
}
*/



	//RUN TESTS

describe("Insect-test 1. Object V (peta- kibi- nano- atto-)", function () {
	it('new L(P.kibi,["kibi","Ki","киби","Ки"])', function () {
		expect(insect('1.65 Mibits')).toEqual('1.65 Mibit');
		expect(insect('5 kibibyte')).toEqual('5 KiB');
		expect(insect('8.5 Kiтесла')).toEqual('8.5 KiT');
		expect(insect('1 кибиБайт -> Байт')).toEqual('1024 B');
		expect(insect('6 КиЗв')).toEqual('6 KiSv');
    });
	
	it('new L(P.mebi,["mebi","Mi","меби","Mи"])', function () {
		expect(insect('1.65 мебиWb')).toEqual('1.65 MiWb');
		expect(insect('1.65 mebipascal')).toEqual('1.65 MiPa');
		expect(insect('1.83 мебилюмен')).toEqual('1.83 Milm');
		expect(insect('0.45 MиФ')).toEqual('0.45 MiF');
	});
	
	it('new L(P.gibi,["gibi","Gi","гиби","Ги"])', function () {
		expect(insect('1.84865168435 gibiтонна')).toEqual('1.84865 Giton');
		expect(insect('2 GiB')).toEqual('2 GiB');
		expect(insect('9.64 гибиБайт')).toEqual('9.64 GiB');
		expect(insect('0.486 Гиангстрем')).toEqual('0.486 GiÅ');
	});

	it('new L(P.tebi,["tebi","Ti","теби","Ти"])', function () {
		expect(insect('6.8 tebiатм')).toEqual('6.8 Tiatm');
		expect(insect('9.46156 Tipixel')).toEqual('9.46156 Tipx');
		expect(insect('864.18 тебиΩ')).toEqual('864.18 TiΩ');
		expect(insect('0.486 Тимоль')).toEqual('0.486 Timol');
	});
	
	it('new L(P.pebi,["pebi","Pi","пеби","Пи"])', function () {
		expect(insect('154.48 pebihertz')).toEqual('154.48 PiHz');
		expect(insect('1 Piгрей')).toEqual('1 PiGy');
		expect(insect('8 пебипарсек')).toEqual('8 Piparsec');
		expect(insect('7.65 Пиsec')).toEqual('7.65 Pis'); //Это не письки, это пебисекунды.
	});

	it('new L(P.exbi,["exbi","Ei","эксби","Эи"])', function () {
		expect(insect('exbibyte')).toEqual('1 EiB');
		expect(insect('EiДжоуль')).toEqual('1 EiJ');
		expect(insect('18 PiWh to эксбиДжоуль')).toEqual('63.2813 EiJ');
		expect(insect('48 эксбигектар')).toEqual('48 Eiha'); //Это никакие не эксгибиционисты.
	});

	it('new L(P.zebi,["zebi","Zi","зеби","Зи"])', function () {
		expect(insect('zebiметр')).toEqual('1 Zim');
		expect(insect('Zigrams')).toEqual('1 Zig'); //это не зига, а зебиграммы
		expect(insect('(1 EiW * 1024) -> зебиватты')).toEqual('1 ZiW'); 						//ok
		expect(insect('8 ЗиЗв')).toEqual('8 ZiSv');
	});
	
	it('new L(P.yobi,["yobi","Yi","йоби","Йи"])', function () {
		expect(insect('yobibyte')).toEqual('1 YiB');
		expect(insect('(2^80) bit -> Yibit')).toEqual('1 Yibit'); 								//это не https://yobit.net
		expect(insect('(14 зебиминут * 100) -> йобичасы')).toEqual('0.0227865 Yih');
		expect(insect('1 Йиlitres to zebiлитры')).toEqual('1024 ZiL');
	});

	it('new L(P.atto,["atto","a","атто","а"])', function () {
		expect(insect('1 attoseconds -> femtoseconds')).toEqual('0.001 fs');
		expect(insect('5 afps')).toEqual('5 aframe/s');
		expect(insect('0.25 аттосекунды')).toEqual('0.25 as');
		expect(insect('6 адж')).toEqual('6 aJ');
	});

	it('new L(P.femto,["femto","f","фемто","ф"])', function () {
		//expect(insect('1 femtometer -> femtomiles')).toEqual('FEMTOMILES NOT WORKING');
		//NOT WORKING NO ONE ENGLISH units with atto-femto-peta...
		
		//	For example...
		//	1 petawatt -> = 1 PW
		//	1 oz -> '1 oz'
		//	1 petaoz -> Unknown identifier: petaoz
		//	1 femtofarad -> 1 fF
		//	1 femtocup -> Unknown identifier: femtocup
		//	8 attoJ->8 aJ
		//	8 attopound -> Unknown identifier: attopound
			
		//	See https://github.com/sharkdp/insect/issues/163
		
		expect(insect('1 femtometer -> miles')).toEqual('6.21371e-19 mi');
		expect(insect('5 пикофарад -> фемтофарады')).toEqual('5000 fF');
		expect(insect('18 фемтоньютон')).toEqual('18 fN');
		expect(insect('6 фsiemens')).toEqual('6 fS');
	});

	it('new L(P.pico,["pico","p","пико","п"])', function () {
		expect(insect('picofarad')).toEqual('1 pF');
		expect(insect('pfarads')).toEqual('1 pF');
		expect(insect('пикофарад')).toEqual('1 pF');
		expect(insect('пФ')).toEqual('1 pF');
	});

	it('new L(P.nano,["nano","n","нано","н"])', function () {
		expect(insect('nanoseconds')).toEqual('1 ns');
		expect(insect('ns')).toEqual('1 ns');
		expect(insect('наносекунда')).toEqual('1 ns');
		expect(insect('нс')).toEqual('1 ns');
	});

	it('new L(P.micro,["micro","u","\u00b5","\u03bc","микро","мк"])', function () {
		expect(insect('micrometer')).toEqual('1 µm');
		expect(insect('\u00b5m')).toEqual('1 µm');
		expect(insect('µm')).toEqual('1 µm');		//'\u00b5'
		expect(insect('\u03bcm')).toEqual('1 µm');
		expect(insect('µm')).toEqual('1 µm');		//'\u03bc'
		expect(insect('микрометр')).toEqual('1 µm');
		expect(insect('мкм')).toEqual('1 µm');
	});

	it('new L(P.milli,["milli","m","милли","м"])', function () {
		expect(insect('milliliter')).toEqual('1 mL');
		expect(insect('ml')).toEqual('1 mL');
		expect(insect('миллилитр')).toEqual('1 mL');
		expect(insect('мл')).toEqual('1 mL');
		
		//an exception - 2 m[м] letter.
		expect(insect('мм -> cm')).toEqual('0.1 cm');	
		expect(insect('mm to meter')).toEqual('0.001 m');	
	});

	it('new L(P.centi,["centi","c","санти","с"])', function () {
		expect(insect('centimeter')).toEqual('1 cm');
		expect(insect('cm')).toEqual('1 cm');
		expect(insect('сантиметр')).toEqual('1 cm');
		expect(insect('см')).toEqual('1 cm');	
	});

	it('new L(P.deci,["deci","d","деци","д"])', function () {
		expect(insect('decimeter')).toEqual('1 dm');
		expect(insect('dm')).toEqual('1 dm');
		expect(insect('дециметр')).toEqual('1 dm');
		expect(insect('дм')).toEqual('1 dm');	
	});
	
	it('new L(P.hecto,["hecto","h","гекто","г"])', function () {
		expect(insect('hectopascal')).toEqual('1 hPa');
		expect(insect('hPa')).toEqual('1 hPa');
		expect(insect('гектопаскаль')).toEqual('1 hPa');
		expect(insect('гПа')).toEqual('1 hPa');	
	});
	
	it('new L(P.kilo,["kilo","k","кило","к"])', function () {
		expect(insect('kilometer')).toEqual('1 km');
		expect(insect('km')).toEqual('1 km');
		expect(insect('километр')).toEqual('1 km');
		expect(insect('км')).toEqual('1 km');	
	});

	it('new L(P.mega,["Mega","mega","M","Мега","мега","М"])', function () {
		expect(insect('MegaJoules')).toEqual('1 MJ');
		expect(insect('megaJoules')).toEqual('1 MJ');
		expect(insect('MJ')).toEqual('1 MJ');
		expect(insect('МегаДжоуль')).toEqual('1 MJ');
		expect(insect('мегаДжоуль')).toEqual('1 MJ');
		expect(insect('МДж')).toEqual('1 MJ');	
	});

	it('new L(P.giga,["Giga","giga","G","гига","Гига","Г"])', function () {
		expect(insect('GigaPa')).toEqual('1 GPa');
		expect(insect('gigaPa')).toEqual('1 GPa');
		expect(insect('GPa')).toEqual('1 GPa');
		expect(insect('ГигаПа')).toEqual('1 GPa');
		expect(insect('гигаПа')).toEqual('1 GPa');
		expect(insect('ГПа')).toEqual('1 GPa');	
	});

	it('new L(P.tera,["tera","T","тера","Т"])', function () {
		expect(insect('terawatt')).toEqual('1 TW');
		expect(insect('TW')).toEqual('1 TW');
		expect(insect('тераватт')).toEqual('1 TW');
		expect(insect('ТВт')).toEqual('1 TW');
	});
	
	it('new L(P.peta,["peta","P","пета","П"])', function () {
		expect(insect('petawatt')).toEqual('1 PW');
		expect(insect('PW')).toEqual('1 PW');
		expect(insect('петаватт')).toEqual('1 PW');
		expect(insect('ПВт')).toEqual('1 PW');
		
	});
	
	it('new L(P.exa,["Exa","exa","E","Экза","экза","Э"])', function () {
		expect(insect('exawatt')).toEqual('1 EW');
		expect(insect('Exawatt')).toEqual('1 EW');
		expect(insect('EW')).toEqual('1 EW');
		expect(insect('экзаватт')).toEqual('1 EW');
		expect(insect('Экзаватт')).toEqual('1 EW');
		expect(insect('ЭВт')).toEqual('1 EW');
	});	
});

describe("Insect-test 2. Object da (phisics units)", function () {
	it('new L(ja.radian,["Radians","radians","Radian","radian","Rad","rad","Радиан","радиан","Рад","рад"])', function () {
		expect(insect('Radians')).toEqual('1 rad');
		expect(insect('radians')).toEqual('1 rad');
		expect(insect('Radian')).toEqual('1 rad');
		expect(insect('radian')).toEqual('1 rad');
		expect(insect('Rad')).toEqual('1 rad');
		expect(insect('rad')).toEqual('1 rad');
		expect(insect('Радиан')).toEqual('1 rad');
		expect(insect('радиан')).toEqual('1 rad');
		expect(insect('Рад')).toEqual('1 rad');
		expect(insect('рад')).toEqual('1 rad');
	});
	
	it('new L(va.degree,["Degrees","degrees","Degree","degree","deg","Deg","\u00b0","Градусов","градусов","Градусы","градусы","Градус","градус"])', function () {
		expect(insect('Degrees')).toEqual('1 °');
		expect(insect('degrees')).toEqual('1 °');
		expect(insect('Degree')).toEqual('1 °');
		expect(insect('degree')).toEqual('1 °');
		expect(insect('deg')).toEqual('1 °');
		expect(insect('Deg')).toEqual('1 °');
		expect(insect('\u00b0')).toEqual('1 °');
		expect(insect('Градус')).toEqual('1 °');
		expect(insect('градус')).toEqual('1 °');
		expect(insect('Градусов')).toEqual('1 °');
		expect(insect('градусов')).toEqual('1 °');
		expect(insect('Градусы')).toEqual('1 °');
		expect(insect('градусы')).toEqual('1 °');
	});
	
	it('new L(ja.hertz,["Hertz","hertz","Hz","Герц","герц","Гц"])', function () {
		expect(insect('Hertz')).toEqual('1 Hz');
		expect(insect('hertz')).toEqual('1 Hz');
		expect(insect('Hz')).toEqual('1 Hz');
		expect(insect('Герц')).toEqual('1 Hz');
		expect(insect('герц')).toEqual('1 Hz');
		expect(insect('Гц')).toEqual('1 Hz');		
	});
	
	it('new L(E.rpm,["RPM","rpm","оборот в минуту","Оборот в минуту","Оборот за минуту","оборотов за минуту","об/мин","Об/мин"])', function () {
		expect(insect('RPM')).toEqual('1 rpm');
		expect(insect('rpm')).toEqual('1 rpm');
		expect(insect('оборот в минуту')).toEqual('1 rpm');
		expect(insect('Оборот в минуту')).toEqual('1 rpm');
		expect(insect('Оборот за минуту')).toEqual('1 rpm');
		expect(insect('оборотов за минуту')).toEqual('1 rpm');
		expect(insect('об/мин')).toEqual('1 rpm');
		expect(insect('Об/мин')).toEqual('1 rpm');
	});
	
	it('new L(ja.newton,["newtons","Newtons","newton","Newton", "N","Ньютонов","ньютонов","Ньютона","ньютона","ньютон","Ньютон","Н"])', function () {
		expect(insect('newton')).toEqual('1 N');
		expect(insect('Newton')).toEqual('1 N');
		expect(insect('newtons')).toEqual('1 N');
		expect(insect('Newtons')).toEqual('1 N');
		expect(insect('N')).toEqual('1 N');
		expect(insect('Ньютонов')).toEqual('1 N');
		expect(insect('ньютонов')).toEqual('1 N');
		expect(insect('Ньютона')).toEqual('1 N');
		expect(insect('ньютона')).toEqual('1 N');
		expect(insect('ньютон')).toEqual('1 N');
		expect(insect('Ньютон')).toEqual('1 N');
		expect(insect('Н')).toEqual('1 N');
	});
	
	it('new L(ja.joule,["joules","Joules","joule","Joule","j","J","джоули","Джоули","джоуль","Джоуль","джоулей","Джоулей","джоуля","Джоуля","дж","Дж"])', function () {
		expect(insect('joules')).toEqual('1 J');
		expect(insect('Joules')).toEqual('1 J');
		expect(insect('joule')).toEqual('1 J');
		expect(insect('Joule')).toEqual('1 J');
		expect(insect('j')).toEqual('1 J');
		expect(insect('J')).toEqual('1 J');
		expect(insect('джоули')).toEqual('1 J');
		expect(insect('Джоули')).toEqual('1 J');
		expect(insect('джоуль')).toEqual('1 J');
		expect(insect('Джоуль')).toEqual('1 J');
		expect(insect('джоулей')).toEqual('1 J');
		expect(insect('Джоулей')).toEqual('1 J');
		expect(insect('джоуля')).toEqual('1 J');
		expect(insect('Джоуля')).toEqual('1 J');
		expect(insect('дж')).toEqual('1 J');
		expect(insect('Дж')).toEqual('1 J');
	});
	
	it('new L(ja.pascal,["Pascals","pascals","Pascal","pascal","Pa","паскаль","Паскаль","паскалей","Паскалей","паскаля","Паскаля","Па"])', function () {
		expect(insect('Pascals')).toEqual('1 Pa');
		expect(insect('pascals')).toEqual('1 Pa');
		expect(insect('Pascal')).toEqual('1 Pa');
		expect(insect('pascal')).toEqual('1 Pa');
		expect(insect('Pa')).toEqual('1 Pa');
		expect(insect('паскаль')).toEqual('1 Pa');
		expect(insect('Паскаль')).toEqual('1 Pa');
		expect(insect('паскалей')).toEqual('1 Pa');
		expect(insect('Паскалей')).toEqual('1 Pa');
		expect(insect('паскаля')).toEqual('1 Pa');
		expect(insect('Паскаля')).toEqual('1 Pa');
		expect(insect('Па')).toEqual('1 Pa');
	});
	
	it('new L(ja.volt,["Volts","volts","Volt","volt","V","вольта","Вольта","вольт","Вольт"]) // + ["В.","В"] after watts ["Вт.","Вт"]', function () {
		expect(insect('Volts')).toEqual('1 V');
		expect(insect('volts')).toEqual('1 V');
		expect(insect('Volt')).toEqual('1 V');
		expect(insect('volt')).toEqual('1 V');
		expect(insect('V')).toEqual('1 V');
		expect(insect('вольта')).toEqual('1 V');
		expect(insect('Вольта')).toEqual('1 V');
		expect(insect('вольт')).toEqual('1 V');
		expect(insect('Вольт')).toEqual('1 V');
		expect(insect('В.')).toEqual('1 V');
		expect(insect('В')).toEqual('1 V');
	});
	
	it('new L(ja.farad,["farads","Farads","farad","Farad","F","фарады","Фарады","фарада","Фарада","фарад","Фарад","Ф"])', function () {
		expect(insect('farads')).toEqual('1 F');
		expect(insect('Farads')).toEqual('1 F');
		expect(insect('farad')).toEqual('1 F');
		expect(insect('Farad')).toEqual('1 F');
		expect(insect('F')).toEqual('1 F');
		expect(insect('фарады')).toEqual('1 F');
		expect(insect('Фарады')).toEqual('1 F');
		expect(insect('фарада')).toEqual('1 F');
		expect(insect('Фарада')).toEqual('1 F');
		expect(insect('фарад')).toEqual('1 F');
		expect(insect('Фарад')).toEqual('1 F');
		expect(insect('Ф')).toEqual('1 F');
	});
	
	it('new L(ja.ohm,["ohms","Ohms","ohm","Ohm","\u03a9","омы","Омы","омa","Омa","ом","Ом"])', function () {
		expect(insect('ohms')).toEqual('1 Ω');
		expect(insect('Ohms')).toEqual('1 Ω');
		expect(insect('ohm')).toEqual('1 Ω');
		expect(insect('Ohm')).toEqual('1 Ω');
		expect(insect('\u03a9')).toEqual('1 Ω');
		expect(insect('омы')).toEqual('1 Ω');
		expect(insect('Омы')).toEqual('1 Ω');
		expect(insect('омa')).toEqual('1 Ω');
		expect(insect('Омa')).toEqual('1 Ω');
		expect(insect('ом')).toEqual('1 Ω');
		expect(insect('Ом')).toEqual('1 Ω');
	});
	
	it('new L(ja.sievert,["sievert","Sievert","Sv","Зиверты","зиверты","Зиверта","зиверта","зиверт","Зиверт","Зв"])', function () {
		expect(insect('sievert')).toEqual('1 Sv');
		expect(insect('Sievert')).toEqual('1 Sv');
		expect(insect('Sv')).toEqual('1 Sv');
		expect(insect('Зиверты')).toEqual('1 Sv');
		expect(insect('зиверты')).toEqual('1 Sv');
		expect(insect('Зиверта')).toEqual('1 Sv');
		expect(insect('зиверта')).toEqual('1 Sv');
		expect(insect('зиверт')).toEqual('1 Sv');
		expect(insect('Зиверт')).toEqual('1 Sv');
		expect(insect('Зв')).toEqual('1 Sv');
	});
	
	it('new L(ja.weber,["Webers","webers","weber","Weber","Wb","веберы","Веберы","вебер","Вебер","Вб"])', function () {
		expect(insect('Webers')).toEqual('1 Wb');
		expect(insect('webers')).toEqual('1 Wb');
		expect(insect('weber')).toEqual('1 Wb');
		expect(insect('Weber')).toEqual('1 Wb');
		expect(insect('Wb')).toEqual('1 Wb');
		expect(insect('веберы')).toEqual('1 Wb');
		expect(insect('Веберы')).toEqual('1 Wb');
		expect(insect('вебер')).toEqual('1 Wb');
		expect(insect('Вебер')).toEqual('1 Wb');
		expect(insect('Вб')).toEqual('1 Wb');
	});
	
	it('new L(ja.tesla,["tesla","Tesla","T","тесла","Тесла","теслы","Теслы","Тл"])', function () {
		expect(insect('tesla')).toEqual('1 T');
		expect(insect('Tesla')).toEqual('1 T');
		expect(insect('T')).toEqual('1 T');
		expect(insect('тесла')).toEqual('1 T');
		expect(insect('Тесла')).toEqual('1 T');
		expect(insect('теслы')).toEqual('1 T');
		expect(insect('Теслы')).toEqual('1 T');
		//expect(insect('Тл')).toEqual('1 T');			//interpreted as Tera liters
	});
	
	it('new L(ja.henry,["henry","Henry","H","генри","Генри","Гн"])', function () {
		expect(insect('henry')).toEqual('1 H');
		expect(insect('Henry')).toEqual('1 H');
		expect(insect('H')).toEqual('1 H');
		expect(insect('генри')).toEqual('1 H');
		expect(insect('Генри')).toEqual('1 H');
		expect(insect('Гн')).toEqual('1 H');
	});
	
	it('new L(ja.coulomb,["coulombs","Coulombs","coulomb","Coulomb","C","кулоны","Кулоны","кулона","Кулона","кулон","Кулон","Кл"])', function () {
		expect(insect('coulombs')).toEqual('1 C');
		expect(insect('Coulombs')).toEqual('1 C');
		expect(insect('coulomb')).toEqual('1 C');
		expect(insect('Coulomb')).toEqual('1 C');
		expect(insect('C')).toEqual('1 C');
		expect(insect('кулоны')).toEqual('1 C');
		expect(insect('Кулоны')).toEqual('1 C');
		expect(insect('кулона')).toEqual('1 C');
		expect(insect('Кулона')).toEqual('1 C');
		expect(insect('кулон')).toEqual('1 C');
		expect(insect('Кулон')).toEqual('1 C');
		expect(insect('Кл')).toEqual('1 C');
	});
	
	it('new L(ja.siemens,["siemens","Siemens","S","Сименс","сименс","См"])', function () {
		expect(insect('siemens')).toEqual('1 S');
		expect(insect('Siemens')).toEqual('1 S');
		expect(insect('S')).toEqual('1 S');
		expect(insect('сименс')).toEqual('1 S');
		expect(insect('Сименс')).toEqual('1 S');
		expect(insect('См')).toEqual('1 S');
	});

	it('new L(ja.lumen,["Lumens","lumens","lumen","Lumen","lm","люмены","Люмены","люмена","Люмена","люмен","Люмен", "Лм"])', function () {
		expect(insect('Lumens')).toEqual('1 lm');
		expect(insect('lumens')).toEqual('1 lm');
		expect(insect('lumen')).toEqual('1 lm');
		expect(insect('lm')).toEqual('1 lm');
		expect(insect('люмен')).toEqual('1 lm');
		expect(insect('Люмен')).toEqual('1 lm');
		expect(insect('люмены')).toEqual('1 lm');
		expect(insect('Люмены')).toEqual('1 lm');
		expect(insect('люмена')).toEqual('1 lm');
		expect(insect('Люмена')).toEqual('1 lm');
		expect(insect('Лм')).toEqual('1 lm');
	});

	it('new L(ja.lux,["lux","Lux","lx","Люксы","люксы","Люкса","люкса","люкс","Люкс","лк"])', function () {
		expect(insect('lux')).toEqual('1 lx');
		expect(insect('Lux')).toEqual('1 lx');
		expect(insect('lx')).toEqual('1 lx');
		expect(insect('Люксы')).toEqual('1 lx');
		expect(insect('люксы')).toEqual('1 lx');
		expect(insect('Люкса')).toEqual('1 lx');
		expect(insect('люкса')).toEqual('1 lx');
		expect(insect('люкс')).toEqual('1 lx');
		expect(insect('Люкс')).toEqual('1 lx');
		expect(insect('лк')).toEqual('1 lx');
	});

	it('new L(ja.becquerel,["Becquerels","becquerels","Becquerel","becquerel","Bq","беккерель","Беккерель","беккерели","Беккерели","беккереля","Беккереля","Бк"])', function () {
		expect(insect('Becquerel')).toEqual('1 Bq');
		expect(insect('becquerel')).toEqual('1 Bq');
		expect(insect('Becquerels')).toEqual('1 Bq');
		expect(insect('becquerels')).toEqual('1 Bq');
		expect(insect('Bq')).toEqual('1 Bq');
		expect(insect('беккерель')).toEqual('1 Bq');
		expect(insect('Беккерель')).toEqual('1 Bq');
		expect(insect('беккерели')).toEqual('1 Bq');
		expect(insect('Беккерели')).toEqual('1 Bq');
		expect(insect('беккереля')).toEqual('1 Bq');
		expect(insect('Беккереля')).toEqual('1 Bq');
		expect(insect('Бк')).toEqual('1 Bq');
	});

	it('new L(ja.gray,["grays","Grays","gray","Gray","Gy","греи","Греи","грей","Грей","грея","Грея","Гр.","Гр"])', function () {
		expect(insect('grays')).toEqual('1 Gy');
		expect(insect('Grays')).toEqual('1 Gy');
		expect(insect('gray')).toEqual('1 Gy');
		expect(insect('Gray')).toEqual('1 Gy');
		expect(insect('Gy')).toEqual('1 Gy');
		expect(insect('греи')).toEqual('1 Gy');
		expect(insect('Греи')).toEqual('1 Gy');
		expect(insect('грей')).toEqual('1 Gy');
		expect(insect('Грей')).toEqual('1 Gy');
		expect(insect('грея')).toEqual('1 Gy');
		expect(insect('Грея')).toEqual('1 Gy');
		expect(insect('Гр.')).toEqual('1 Gy');
		expect(insect('Гр')).toEqual('1 Gy');
	});
	
	it('new L(ja.katal,["katal","Katal","kat","Kat","катал","Катал","кат","Кат"])', function () {
		expect(insect('katal')).toEqual('1 kat');
		expect(insect('Katal')).toEqual('1 kat');
		expect(insect('kat')).toEqual('1 kat');
		expect(insect('Kat')).toEqual('1 kat');
		expect(insect('катал')).toEqual('1 kat');
		expect(insect('Катал')).toEqual('1 kat');
		expect(insect('кат')).toEqual('1 kat');
		expect(insect('Кат')).toEqual('1 kat');
	});
	
	it('new L(va.hectare,["hectare","Hectare","ha","Ha","гектар","Гектар","га","Га"])', function () {
		expect(insect('hectare')).toEqual('1 ha');
		expect(insect('Hectare')).toEqual('1 ha');
		expect(insect('ha')).toEqual('1 ha');
		expect(insect('Ha')).toEqual('1 ha');
		expect(insect('гектар')).toEqual('1 ha');
		expect(insect('Гектар')).toEqual('1 ha');
		expect(insect('га')).toEqual('1 ha');
		expect(insect('Га')).toEqual('1 ha');
	});
	
	it('new L(va.tonne,["tonnes","Tonnes","tonne","Tonne","tons","Tons","ton","Ton","тонна","Тонна","тонн","Тонн","тонны","Тонны","т.","т"])', function () {
		expect(insect('tonnes')).toEqual('1 ton');
		expect(insect('Tonnes')).toEqual('1 ton');
		expect(insect('tonne')).toEqual('1 ton');
		expect(insect('Tonne')).toEqual('1 ton');
		expect(insect('tons')).toEqual('1 ton');
		expect(insect('Tons')).toEqual('1 ton');
		expect(insect('ton')).toEqual('1 ton');
		expect(insect('Ton')).toEqual('1 ton');
		expect(insect('тонна')).toEqual('1 ton');
		expect(insect('Тонна')).toEqual('1 ton');
		expect(insect('тонн')).toEqual('1 ton');
		expect(insect('Тонн')).toEqual('1 ton');
		expect(insect('тонны')).toEqual('1 ton');
		expect(insect('Тонны')).toEqual('1 ton');
		expect(insect('т.')).toEqual('1 ton');
		expect(insect('т')).toEqual('1 ton');
	});
	
	it('new L(va.electronvolt,["electronvolt","Electronvolt","eV","электронвольты","Электронвольты","электрон-вольты","Электрон-вольты","электрон-вольт","Электрон-вольт","электронвольт","Электронвольт","эВ"])', function () {
		expect(insect('electronvolt')).toEqual('1 eV');
		expect(insect('Electronvolt')).toEqual('1 eV');
		expect(insect('eV')).toEqual('1 eV');
		expect(insect('электронвольты')).toEqual('1 eV');
		expect(insect('Электронвольты')).toEqual('1 eV');
		expect(insect('электрон-вольты')).toEqual('1 eV');
		expect(insect('Электрон-вольты')).toEqual('1 eV');
		expect(insect('электрон-вольт')).toEqual('1 eV');
		expect(insect('Электрон-вольт')).toEqual('1 eV');
		expect(insect('электронвольт')).toEqual('1 eV');
		expect(insect('Электронвольт')).toEqual('1 eV');
		expect(insect('эВ')).toEqual('1 eV');
	});

	it('new L(E.calorie,["calories","Сalories","calorie","Сalorie","cal","Сal","калорий","Калорий","калории","Калории","калория","Калория","кал.","Кал.","кал","Кал"])', function () {
		expect(insect('calories')).toEqual('1 cal');
		expect(insect('Сalories')).toEqual('1 cal');
		expect(insect('calorie')).toEqual('1 cal');
		expect(insect('Сalorie')).toEqual('1 cal');
		expect(insect('cal')).toEqual('1 cal');
		expect(insect('Сal')).toEqual('1 cal');
		expect(insect('калорий')).toEqual('1 cal');
		expect(insect('Калорий')).toEqual('1 cal');
		expect(insect('калории')).toEqual('1 cal');
		expect(insect('Калории')).toEqual('1 cal');
		expect(insect('калория')).toEqual('1 cal');
		expect(insect('Калория')).toEqual('1 cal');
		expect(insect('кал.')).toEqual('1 cal');
		expect(insect('Кал.')).toEqual('1 cal');
		expect(insect('кал')).toEqual('1 cal');
		expect(insect('Кал')).toEqual('1 cal');
	});
	
	it('new L(va.bel,["bel","Bel","бел","Бел"])', function () {
		expect(insect('bel')).toEqual('1 bel');
		expect(insect('Bel')).toEqual('1 bel');
		expect(insect('бел')).toEqual('1 bel');
		expect(insect('Бел')).toEqual('1 bel');
	});
	
	it('new L(va.astronomicalUnit,["AU","au","a.u.","ua","а.е."])', function () {
		expect(insect('AU')).toEqual('1 AU');
		expect(insect('au')).toEqual('1 AU');
		expect(insect('ua')).toEqual('1 AU');
		expect(insect('а.е.')).toEqual('1 AU');
	});
	
	it('new L(da.parsec,["parsecs","Parsecs","parsec","Parsec","pc","парсеков","Парсеков","парсека","Парсека","парсек","Парсек","пк"])', function () {
		expect(insect('parsecs')).toEqual('1 parsec');
		expect(insect('Parsecs')).toEqual('1 parsec');
		expect(insect('parsec')).toEqual('1 parsec');
		expect(insect('Parsec')).toEqual('1 parsec');
		expect(insect('pc')).toEqual('1 parsec');
		expect(insect('парсеков')).toEqual('1 parsec');
		expect(insect('Парсеков')).toEqual('1 parsec');
		expect(insect('парсека')).toEqual('1 parsec');
		expect(insect('Парсека')).toEqual('1 parsec');
		expect(insect('парсек')).toEqual('1 parsec');
		expect(insect('Парсек')).toEqual('1 parsec');
		expect(insect('пк')).toEqual('1 parsec');
	});
	
	it('new L(da.lightyear,["lightyears","Lightyears","lightyear","Lightyear","ly","световых лет","св.лет", "св. лет","световой год","св.год","св. год"])', function () {
		expect(insect('lightyears')).toEqual('1 ly');
		expect(insect('Lightyears')).toEqual('1 ly');
		expect(insect('lightyear')).toEqual('1 ly');
		expect(insect('Lightyear')).toEqual('1 ly');
		expect(insect('ly')).toEqual('1 ly');
		expect(insect('световых лет')).toEqual('1 ly');
		expect(insect('св.лет')).toEqual('1 ly');
		expect(insect('св. лет')).toEqual('1 ly');
		expect(insect('световой год')).toEqual('1 ly');
		expect(insect('св.год')).toEqual('1 ly');
		expect(insect('св. год')).toEqual('1 ly');
	});
	
	it('new L(va.barn,["barn","Barn","барн","Барн","бн",+"б"]) //+ б after бар and байт', function () {
		expect(insect('barn')).toEqual('1 barn');
		expect(insect('Barn')).toEqual('1 barn');
		expect(insect('барн')).toEqual('1 barn');
		expect(insect('Барн')).toEqual('1 barn');
		expect(insect('бн')).toEqual('1 barn');
		expect(insect('б')).toEqual('1 barn');
	});
	
	it('new L(va.bar,["bar","Bar","бар","Бар"])', function () {
		expect(insect('bar')).toEqual('1 bar');
		expect(insect('Bar')).toEqual('1 bar');
		expect(insect('бар')).toEqual('1 bar');
		expect(insect('Бар')).toEqual('1 bar');
	});
	
	it('new L(va.angstrom,["angstrom","Angstrom","\u00c5","ангстрем","Ангстрем"])', function () {
		expect(insect('angstrom')).toEqual('1 Å');
		expect(insect('Angstrom')).toEqual('1 Å');
		expect(insect('\u00c5')).toEqual('1 Å');
		expect(insect('ангстрем')).toEqual('1 Å');
		expect(insect('Ангстрем')).toEqual('1 Å');
	});
	
	it('new L(ea.gauss,["gauss","Gauss"])//,"гаусс","Гаусс","Гс"]) //Гс - interpreted as giga-second, гаусс and Гаусс show an error.', function () {
		expect(insect('gauss')).toEqual('1 gauss');
		expect(insect('Gauss')).toEqual('1 gauss');
	});
	
	it('new L(G.ampere,["amperes","Amperes","ampere","Ampere","A","амперы","Амперы","Ампер","ампер","А"])', function () {
		expect(insect('amperes')).toEqual('1 A');
		expect(insect('Amperes')).toEqual('1 A');
		expect(insect('ampere')).toEqual('1 A');
		expect(insect('Ampere')).toEqual('1 A');
		expect(insect('A')).toEqual('1 A');
		expect(insect('амперы')).toEqual('1 A');
		expect(insect('Амперы')).toEqual('1 A');
		expect(insect('Ампер')).toEqual('1 A');
		expect(insect('ампер')).toEqual('1 A');
		expect(insect('А')).toEqual('1 A');
	});
	
	it('new L(G.mole,["mole","Mole","mol","Mol","моль","Моль"])', function () {
		expect(insect('mole')).toEqual('1 mol');
		expect(insect('Mole')).toEqual('1 mol');
		expect(insect('mol')).toEqual('1 mol');
		expect(insect('Mol')).toEqual('1 mol');
		expect(insect('моль')).toEqual('1 mol');
		expect(insect('Моль')).toEqual('1 mol');
	});
	
	it('new L(G.kelvin,["kelvin","Kelvin","K","кельвин","Кельвин","кельвинов","Кельвинов","К"])', function () {
		expect(insect('kelvin')).toEqual('1 K');
		expect(insect('Kelvin')).toEqual('1 K');
		expect(insect('K')).toEqual('1 K');
		expect(insect('кельвин')).toEqual('1 K');
		expect(insect('Кельвин')).toEqual('1 K');
		expect(insect('кельвинов')).toEqual('1 K');
		expect(insect('Кельвинов')).toEqual('1 K');
		expect(insect('К')).toEqual('1 K');
	});
	
	it('new L(G.candela,["candela","Candela","cd","канделы","Канделы","Кандела","кандела","кандел","Кандел","кд"])', function () {
		expect(insect('candela')).toEqual('1 cd');
		expect(insect('Candela')).toEqual('1 cd');
		expect(insect('cd')).toEqual('1 cd');
		expect(insect('канделы')).toEqual('1 cd');
		expect(insect('Канделы')).toEqual('1 cd');
		expect(insect('Кандела')).toEqual('1 cd');
		expect(insect('кандела')).toEqual('1 cd');
		expect(insect('кандел')).toEqual('1 cd');
		expect(insect('Кандел')).toEqual('1 cd');
		//expect(insect('кд')).toEqual('1 cd');			//interpreted as kilo day and return kd !== cd.
	});
	
	it('new L(X.append(P.semigroupDerivedUnit)(ja.watt)(u.hour),["Wh","wh","Watt-hour","watt-hour","ватт-часы","Вт час","Вт-ч","Втч","вт\u22C5ч"])', function () {
		expect(insect('Wh')).toEqual('1 W·h');
		expect(insect('wh')).toEqual('1 W·h');
		expect(insect('Watt-hour')).toEqual('1 W·h');
		expect(insect('watt-hour')).toEqual('1 W·h');
		expect(insect('ватт-часы')).toEqual('1 W·h');
		expect(insect('Вт час')).toEqual('1 W·h');
		expect(insect('Вт-ч')).toEqual('1 W·h');
		expect(insect('Втч')).toEqual('1 W·h');
		expect(insect('вт\u22C5ч')).toEqual('1 W·h');
	});
	
	it('new L(X.append(P.semigroupDerivedUnit)(ja.watt)(u.hour),["Wh","wh","Watt-hour","watt-hour","ватт-часы","Вт час","Вт-ч","Втч","вт\u22C5ч"])', function () {
		expect(insect('Wh')).toEqual('1 W·h');
		expect(insect('wh')).toEqual('1 W·h');
		expect(insect('Watt-hour')).toEqual('1 W·h');
		expect(insect('watt-hour')).toEqual('1 W·h');
		expect(insect('ватт-часы')).toEqual('1 W·h');
		expect(insect('Вт час')).toEqual('1 W·h');
		expect(insect('Вт-ч')).toEqual('1 W·h');
		expect(insect('Втч')).toEqual('1 W·h');
		expect(insect('вт\u22C5ч')).toEqual('1 W·h');
	});
	

	it('new L(ja.watt,["watts","Watts","watt","Watt","W","ватты","Ватты","Ватта","ватта","ватт","Ватт","Вт.","Вт"])', function () {
		expect(insect('watts')).toEqual('1 W');
		expect(insect('Watts')).toEqual('1 W');
		expect(insect('watt')).toEqual('1 W');
		expect(insect('Watt')).toEqual('1 W');
		expect(insect('W')).toEqual('1 W');
		expect(insect('ватты')).toEqual('1 W');
		expect(insect('Ватта')).toEqual('1 W');
		expect(insect('Ватта')).toEqual('1 W');
		expect(insect('ватта')).toEqual('1 W');
		expect(insect('ватт')).toEqual('1 W');
		expect(insect('Ватт')).toEqual('1 W');
		expect(insect('Вт.')).toEqual('1 W');
		expect(insect('Вт')).toEqual('1 W');
	});
	//volt ["В.", "В"] already tested...
	
	it('new L(fa["byte"],"Bytes bytes Byte byte B Octets octets Octet octet Байты байты Байтов байтов Байта байта байт Байт Б".split(" "))', function () {
		expect(insect('Bytes')).toEqual('1 B');
		expect(insect('bytes')).toEqual('1 B');
		expect(insect('Byte')).toEqual('1 B');
		expect(insect('byte')).toEqual('1 B');
		expect(insect('B')).toEqual('1 B');
		expect(insect('Octets')).toEqual('1 B');
		expect(insect('octets')).toEqual('1 B');
		expect(insect('Octet')).toEqual('1 B');
		expect(insect('octet')).toEqual('1 B');
		expect(insect('Байты')).toEqual('1 B');
		expect(insect('байты')).toEqual('1 B');
		expect(insect('Байтов')).toEqual('1 B');
		expect(insect('байтов')).toEqual('1 B');
		expect(insect('Байта')).toEqual('1 B');
		expect(insect('байта')).toEqual('1 B');
		expect(insect('байт')).toEqual('1 B');
		expect(insect('Байт')).toEqual('1 B');
		expect(insect('Б')).toEqual('1 B');
	});
	
	it('new L(fa.bit,["bits","Bits","bit","Bit","битов","Битов","бита","Бита","бит","Бит"])', function () {
		expect(insect('bits')).toEqual('1 bit');
		expect(insect('Bits')).toEqual('1 bit');
		expect(insect('bit')).toEqual('1 bit');
		expect(insect('Bit')).toEqual('1 bit');
		expect(insect('битов')).toEqual('1 bit');
		expect(insect('Битов')).toEqual('1 bit');
		expect(insect('бита')).toEqual('1 bit');
		expect(insect('Бита')).toEqual('1 bit');
		expect(insect('бит')).toEqual('1 bit');
		expect(insect('Бит')).toEqual('1 bit');
	});
	
	it('new L(P.divideUnits(fa.bit)(G.second),["bps","bits per second","бит в секунду","Бит в секунду","бит/s","Бит/s","бит/сек","Бит/сек","бит/с","Бит/с","бит за секунду","Бит за секунду"])', function () {
		expect(insect('bps')).toEqual('1 bit/s');
		expect(insect('bits per second')).toEqual('1 bit/s');
		expect(insect('бит в секунду')).toEqual('1 bit/s');
		expect(insect('Бит в секунду')).toEqual('1 bit/s');
		expect(insect('бит/s')).toEqual('1 bit/s');
		expect(insect('Бит/s')).toEqual('1 bit/s');
		expect(insect('бит/сек')).toEqual('1 bit/s');
		expect(insect('Бит/сек')).toEqual('1 bit/s');
		expect(insect('бит/с')).toEqual('1 bit/s');
		expect(insect('Бит/с')).toEqual('1 bit/s');
		expect(insect('бит за секунду')).toEqual('1 bit/s');
		expect(insect('Бит за секунду')).toEqual('1 bit/s');
	});
	//barn [б] already tested
	
	//test flick if this will be added...
	//it('new L(G.flick,["flick","flicks"])', function () {
	//	expect(insect('flick')).toEqual('1 flick');
	//	expect(insect('flicks')).toEqual('1 flick');
	//});
	
	it('new L(G.second,["seconds","Seconds","second","Second","sec","Sec","s","секунды","Секунды",\
	"секунда","Секунда","секунд","Секунд","в секунду","В секунду","за секунду","За секунду",\
	"за сек","За сек","в сек","В сек","сек","Сек","с.","с"])', function () {
		expect(insect('seconds')).toEqual('1 s');
		expect(insect('Seconds')).toEqual('1 s');
		expect(insect('second')).toEqual('1 s');
		expect(insect('Second')).toEqual('1 s');
		expect(insect('sec')).toEqual('1 s');
		expect(insect('Sec')).toEqual('1 s');
		expect(insect('s')).toEqual('1 s');
		expect(insect('секунды')).toEqual('1 s');
		expect(insect('Секунды')).toEqual('1 s');
		expect(insect('секунда')).toEqual('1 s');
		expect(insect('Секунда')).toEqual('1 s');
		expect(insect('секунд')).toEqual('1 s');
		expect(insect('Секунд')).toEqual('1 s');
		expect(insect('в секунду')).toEqual('1 s');
		expect(insect('В секунду')).toEqual('1 s');
		expect(insect('за секунду')).toEqual('1 s');
		expect(insect('За секунду')).toEqual('1 s');
		expect(insect('за сек')).toEqual('1 s');
		expect(insect('За сек')).toEqual('1 s');
		expect(insect('в сек')).toEqual('1 s');
		expect(insect('В сек')).toEqual('1 s');
		expect(insect('сек')).toEqual('1 s');
		expect(insect('Сек')).toEqual('1 s');
		expect(insect('с.')).toEqual('1 s');
		expect(insect('с')).toEqual('1 s');
	});

	it('new L(u.minute,["minutes","Minutes","minute","Minute","min","Min","минуты","Минуты","минута","Минута","минут","Минут","мин.","Мин.","мин","Мин"])', function () {
		expect(insect('minutes')).toEqual('1 min');
		expect(insect('Minutes')).toEqual('1 min');
		expect(insect('minute')).toEqual('1 min');
		expect(insect('Minute')).toEqual('1 min');
		expect(insect('min')).toEqual('1 min');
		expect(insect('Min')).toEqual('1 min');
		expect(insect('минуты')).toEqual('1 min');
		expect(insect('минуты')).toEqual('1 min');
		expect(insect('минут')).toEqual('1 min');
		expect(insect('Минут')).toEqual('1 min');
		expect(insect('минута')).toEqual('1 min');
		expect(insect('Минута')).toEqual('1 min');
		expect(insect('мин.')).toEqual('1 min');
		expect(insect('Мин.')).toEqual('1 min');
		expect(insect('мин')).toEqual('1 min');
		expect(insect('Мин')).toEqual('1 min');
	});
	
	it('new L(u.hour,["hours","Hours","hour","Hour","h","часов","Часов","часы","Часы","часа","Часа","час","Час","ч.","ч"])', function () {
		expect(insect('hours')).toEqual('1 h');
		expect(insect('Hours')).toEqual('1 h');
		expect(insect('hour')).toEqual('1 h');
		expect(insect('Hour')).toEqual('1 h');
		expect(insect('h')).toEqual('1 h');
		expect(insect('часов')).toEqual('1 h');
		expect(insect('Часов')).toEqual('1 h');
		expect(insect('часы')).toEqual('1 h');
		expect(insect('Часы')).toEqual('1 h');
		expect(insect('часа')).toEqual('1 h');
		expect(insect('Часа')).toEqual('1 h');
		expect(insect('час')).toEqual('1 h');
		expect(insect('Час')).toEqual('1 h');
		expect(insect('ч.')).toEqual('1 h');
		expect(insect('ч')).toEqual('1 h');
	});
	
	it('new L(u.day,["days","Days","day","Day","дни","Дни","дней","Дней","день","День","ДН.","дн","д.","д","сут.","суток"])', function () {
		expect(insect('days')).toEqual('1 d');
		expect(insect('Days')).toEqual('1 d');
		expect(insect('day')).toEqual('1 d');
		expect(insect('Day')).toEqual('1 d');
		expect(insect('дни')).toEqual('1 d');
		expect(insect('Дни')).toEqual('1 d');
		expect(insect('дней')).toEqual('1 d');
		expect(insect('Дней')).toEqual('1 d');
		expect(insect('день')).toEqual('1 d');
		expect(insect('День')).toEqual('1 d');
		expect(insect('д.')).toEqual('1 d');
		expect(insect('д')).toEqual('1 d');
		expect(insect('ДН.')).toEqual('1 d');
		expect(insect('дн')).toEqual('1 d');
		expect(insect('сут.')).toEqual('1 d');
		expect(insect('суток')).toEqual('1 d');
	});
	
	it('new L(u.week,["weeks","Weeks","week","Week","недели","Недели","Недель","недель","неделя","Неделя"])', function () {
		expect(insect('weeks')).toEqual('1 week');
		expect(insect('Weeks')).toEqual('1 week');
		expect(insect('week')).toEqual('1 week');
		expect(insect('Week')).toEqual('1 week');
		expect(insect('недели')).toEqual('1 week');
		expect(insect('Недели')).toEqual('1 week');
		expect(insect('Недель')).toEqual('1 week');
		expect(insect('недель')).toEqual('1 week');
		expect(insect('неделя')).toEqual('1 week');
		expect(insect('Неделя')).toEqual('1 week');
	});
	
	it('new L(E.fortnight,["fortnights","fortnight"]) //do not know russian equivalent', function () {
		expect(insect('fortnights')).toEqual('1 fortnight');
		expect(insect('fortnight')).toEqual('1 fortnight');
	});
	
	it('new L(u.month,["months","Months","month","Month","месяцев","Месяцев","месяц","Месяц","мес.","мес"])', function () {
		expect(insect('months')).toEqual('1 month');
		expect(insect('Months')).toEqual('1 month');
		expect(insect('month')).toEqual('1 month');
		expect(insect('Month')).toEqual('1 month');
		expect(insect('месяцев')).toEqual('1 month');
		expect(insect('Месяцев')).toEqual('1 month');
		expect(insect('месяц')).toEqual('1 month');
		expect(insect('Месяц')).toEqual('1 month');
		expect(insect('мес.')).toEqual('1 month');
		expect(insect('мес')).toEqual('1 month');
	});
	
	//it('new L(u.quarter,["quarter","Quarter","quarter-year","Quarter-year","квартал","Квартал","кварталы","Кварталы","кварталов","Кварталов"])\
	//	1/4 from a year ~ 4 mounths... u.quarter not defined.', function () {
	//	expect(insect('quarter')).toEqual('1 quarter');
	//	expect(insect('Quarter')).toEqual('1 quarter');
	//	expect(insect('quarter-year')).toEqual('1 quarter');
	//	expect(insect('Quarter-year')).toEqual('1 quarter');
	//	expect(insect('квартал')).toEqual('1 quarter');
	//	expect(insect('Квартал')).toEqual('1 quarter');
	//	expect(insect('кварталы')).toEqual('1 quarter');
	//	expect(insect('Кварталы')).toEqual('1 quarter');
	//	expect(insect('кварталов')).toEqual('1 quarter');
	//	expect(insect('Кварталов')).toEqual('1 quarter');
	//});
	
	it('new L(u.year,["years","Years","year","Year","лет","Лет","год","Год"])', function () {
		expect(insect('years')).toEqual('1 year');
		expect(insect('Years')).toEqual('1 year');
		expect(insect('year')).toEqual('1 year');
		expect(insect('Year')).toEqual('1 year');
		expect(insect('лет')).toEqual('1 year');
		expect(insect('Лет')).toEqual('1 year');
		expect(insect('год')).toEqual('1 year');
		expect(insect('Год')).toEqual('1 year');
	});
	
	it('new L(G.gram,["grammes","Grammes","gramme","Gramme","grams","Grams","gram","Gram","g","граммы","Граммы","граммов","Граммов","грамм","Грамм","г.","г"])', function () {
		expect(insect('grammes')).toEqual('1 g');
		expect(insect('Grammes')).toEqual('1 g');
		expect(insect('gramme')).toEqual('1 g');
		expect(insect('Gramme')).toEqual('1 g');
		expect(insect('grams')).toEqual('1 g');
		expect(insect('Grams')).toEqual('1 g');
		expect(insect('gram')).toEqual('1 g');
		expect(insect('Gram')).toEqual('1 g');
		expect(insect('g')).toEqual('1 g');
		expect(insect('граммы')).toEqual('1 g');
		expect(insect('Граммы')).toEqual('1 g');
		expect(insect('граммов')).toEqual('1 g');
		expect(insect('Граммов')).toEqual('1 g');
		expect(insect('грамм')).toEqual('1 g');
		expect(insect('Грамм')).toEqual('1 g');
		expect(insect('г.')).toEqual('1 g');
		expect(insect('г')).toEqual('1 g');		
	});
	
	it('new L(G.meter,["metres","Metres","metre","Metre","meters","Meters","meter","Meter","m","метры","Метры","метров","Метров","метр","Метр","м.","м"])', function () {
		expect(insect('metres')).toEqual('1 m');
		expect(insect('Metres')).toEqual('1 m');
		expect(insect('metre')).toEqual('1 m');
		expect(insect('Metre')).toEqual('1 m');
		expect(insect('meters')).toEqual('1 m');
		expect(insect('Meters')).toEqual('1 m');
		expect(insect('meter')).toEqual('1 m');
		expect(insect('Meter')).toEqual('1 m');
		expect(insect('m')).toEqual('1 m');
		expect(insect('метры')).toEqual('1 m');
		expect(insect('Метры')).toEqual('1 m');
		expect(insect('метров')).toEqual('1 m');
		expect(insect('Метров')).toEqual('1 m');
		expect(insect('метр')).toEqual('1 m');
		expect(insect('Метр')).toEqual('1 m');
		expect(insect('м.')).toEqual('1 m');
		expect(insect('м')).toEqual('1 m');
	});
	
	it('new L(va.liter,"liters liter litres litre L l литров Литров литры Литры литр Литр л. л".split(" "))', function () {
		expect(insect('liters')).toEqual('1 L');
		expect(insect('liter')).toEqual('1 L');
		expect(insect('litres')).toEqual('1 L');
		expect(insect('litre')).toEqual('1 L');
		expect(insect('L')).toEqual('1 L');
		expect(insect('l')).toEqual('1 L');
		expect(insect('литров')).toEqual('1 L');
		expect(insect('Литров')).toEqual('1 L');
		expect(insect('литры')).toEqual('1 L');
		expect(insect('Литры')).toEqual('1 L');
		expect(insect('литр')).toEqual('1 L');
		expect(insect('Литр')).toEqual('1 L');
		expect(insect('л.')).toEqual('1 L');
		expect(insect('л')).toEqual('1 L');
	});
	
	it('new L(E.atm,["atm.","Atm.","atm","Atm","атмосферы","Атмосферы","атмосфера","Атмосфера","атмосфер","Aтмосфер","атм.","атм"])', function () {
		expect(insect('atm.')).toEqual('1 atm');
		expect(insect('Atm.')).toEqual('1 atm');
		expect(insect('atm')).toEqual('1 atm');
		expect(insect('Atm')).toEqual('1 atm');
		expect(insect('атмосферы')).toEqual('1 atm');
		expect(insect('Атмосферы')).toEqual('1 atm');
		expect(insect('атмосфера')).toEqual('1 atm');
		expect(insect('Атмосфера')).toEqual('1 atm');
		expect(insect('атмосфер')).toEqual('1 atm');
		expect(insect('Aтмосфер')).toEqual('1 atm');
		expect(insect('атм.')).toEqual('1 atm');
		expect(insect('атм')).toEqual('1 atm');
	});
	
	it('new L(E.pixel,["pixels","Pixels","pixel","Pixel","px","pel","пикселей","Пикселей","пиксель","Пиксель","пиксел","Пиксел"])', function () {
		expect(insect('pixels')).toEqual('1 px');
		expect(insect('Pixels')).toEqual('1 px');
		expect(insect('pixel')).toEqual('1 px');
		expect(insect('Pixel')).toEqual('1 px');
		expect(insect('px')).toEqual('1 px');
		expect(insect('pel')).toEqual('1 px');
		expect(insect('пикселей')).toEqual('1 px');
		expect(insect('Пикселей')).toEqual('1 px');
		expect(insect('пиксель')).toEqual('1 px');
		expect(insect('Пиксель')).toEqual('1 px');
		expect(insect('пиксел')).toEqual('1 px');
		expect(insect('Пиксел')).toEqual('1 px');
	});
	
	it('new L(E.frame,["frames","Frames","frame","Frame","фреймов","Фреймов","фрейм","Фрейм"])', function () {
		expect(insect('frames')).toEqual('1 frame');
		expect(insect('Frames')).toEqual('1 frame');
		expect(insect('frame')).toEqual('1 frame');
		expect(insect('Frame')).toEqual('1 frame');
		expect(insect('фреймов')).toEqual('1 frame');
		expect(insect('Фреймов')).toEqual('1 frame');
		expect(insect('фрейм')).toEqual('1 frame');
		expect(insect('Фрейм')).toEqual('1 frame');
	});
	
	it('new L(P.divideUnits(E.frame)(G.second),["frames per second","FPS","fps","фреймов в секунду","фпс","ФПС"])', function () {
		expect(insect('frames per second')).toEqual('1 frame/s');
		expect(insect('FPS')).toEqual('1 frame/s');
		expect(insect('fps')).toEqual('1 frame/s');
		expect(insect('фреймов в секунду')).toEqual('1 frame/s');
		expect(insect('фпс')).toEqual('1 frame/s');
		expect(insect('ФПС')).toEqual('1 frame/s');
	});
	
	it('new L(E.dot,["dots","Dots","dot","Dot","точек","Точек","точка","Точка"])', function () {
		expect(insect('dots')).toEqual('1 dot');
		expect(insect('Dots')).toEqual('1 dot');
		expect(insect('dot')).toEqual('1 dot');
		expect(insect('Dot')).toEqual('1 dot');
		expect(insect('точек')).toEqual('1 dot');
		expect(insect('Точек')).toEqual('1 dot');
		expect(insect('точка')).toEqual('1 dot');
		expect(insect('Точка')).toEqual('1 dot');
	});
});

describe("Insect-test 3. Object P (British Units)", function () {

	it('new L(P.divideUnits(h.mile)(u.hour),["miles per hour", "mph","m/h","миль/с","миль в секунду"])', function () {
		expect(insect('miles per hour')).toEqual('1 mi/h');
		expect(insect('mph')).toEqual('1 mi/h');
		expect(insect('m/h')).toEqual('1 mi/h');
		expect(insect('миль/с')).toEqual('1 mi/h');
		expect(insect('миль в секунду')).toEqual('1 mi/h');
    });

	it('new L(h.mile,["miles","Miles","mile","Mile","мили","Мили","миль","Mиль","миля","Миля"])', function () {
		expect(insect('miles')).toEqual('1 mi');
		expect(insect('Miles')).toEqual('1 mi');
		expect(insect('mile')).toEqual('1 mi');
		expect(insect('Mile')).toEqual('1 mi');
		expect(insect('мили')).toEqual('1 mi');
		expect(insect('Мили')).toEqual('1 mi');
		expect(insect('миль')).toEqual('1 mi');
		expect(insect('Mиль')).toEqual('1 mi');
		expect(insect('миля')).toEqual('1 mi');
		expect(insect('Миля')).toEqual('1 mi');
    });
	
	it('new L(h.inch,["inches","Inches","inch","Inch","in","In","дюймов","Дюймов","дюйм","Дюйм","\u2033"])', function () {
		expect(insect('inches')).toEqual('1 in');
		expect(insect('Inches')).toEqual('1 in');
		expect(insect('inch')).toEqual('1 in');
		expect(insect('Inch')).toEqual('1 in');
		expect(insect('in')).toEqual('1 in');
		expect(insect('In')).toEqual('1 in');
		expect(insect('дюймов')).toEqual('1 in');
		expect(insect('Дюймов')).toEqual('1 in');
		expect(insect('дюйм')).toEqual('1 in');
		expect(insect('Дюйм')).toEqual('1 in');
		expect(insect('\u2033')).toEqual('1 in');
    });
	
	it('new L(h.yard,["yards","Yards","yard","Yard","yd","Yd","ярдов","Ярдов","ярд","Ярд"])', function () {
		expect(insect('yards')).toEqual('1 yd');
		expect(insect('Yards')).toEqual('1 yd');
		expect(insect('yard')).toEqual('1 yd');
		expect(insect('Yard')).toEqual('1 yd');
		expect(insect('yd')).toEqual('1 yd');
		expect(insect('Yd')).toEqual('1 yd');
		expect(insect('ярдов')).toEqual('1 yd');
		expect(insect('Ярдов')).toEqual('1 yd');
		expect(insect('ярд')).toEqual('1 yd');
		expect(insect('Ярд')).toEqual('1 yd');
    });

	it('new L(h.foot,["feet","Feet","foot","Foot","ft","Ft","футов","Футов","фут","Фут","\u02B9"])', function () {
		expect(insect('feet')).toEqual('1 ft');
		expect(insect('Feet')).toEqual('1 ft');
		expect(insect('foot')).toEqual('1 ft');
		expect(insect('Foot')).toEqual('1 ft');
		expect(insect('ft')).toEqual('1 ft');
		expect(insect('Ft')).toEqual('1 ft');
		expect(insect('футов')).toEqual('1 ft');
		expect(insect('Футов')).toEqual('1 ft');
		expect(insect('фут')).toEqual('1 ft');
		expect(insect('Фут')).toEqual('1 ft');
		expect(insect('\u02B9')).toEqual('1 ft');
    });
	
	it('new L(h.ounce,["ounces","Ounces","ounce","Ounce","oz","Oz","унции","Унции","унций","Унций","унция","Унция"])', function () {
		expect(insect('ounces')).toEqual('1 oz');
		expect(insect('Ounces')).toEqual('1 oz');
		expect(insect('ounce')).toEqual('1 oz');
		expect(insect('Ounce')).toEqual('1 oz');
		expect(insect('oz')).toEqual('1 oz');
		expect(insect('Oz')).toEqual('1 oz');
		expect(insect('унции')).toEqual('1 oz');
		expect(insect('Унции')).toEqual('1 oz');
		expect(insect('унций')).toEqual('1 oz');
		expect(insect('Унций')).toEqual('1 oz');
		expect(insect('унция')).toEqual('1 oz');
		expect(insect('Унция')).toEqual('1 oz');
    });
	
	it('new L(E.lbf,["Pound_force","pound_force","lbf","Lbf","фунт-сила","Фунт-сила","фунт-силы","Фунт-силы","фунт-сил","Фунт-сил"])', function () {
		expect(insect('Pound_force')).toEqual('1 lbf');
		expect(insect('pound_force')).toEqual('1 lbf');
		expect(insect('lbf')).toEqual('1 lbf');
		expect(insect('Lbf')).toEqual('1 lbf');
		expect(insect('фунт-сила')).toEqual('1 lbf');
		expect(insect('Фунт-сила')).toEqual('1 lbf');
		expect(insect('фунт-силы')).toEqual('1 lbf');
		expect(insect('Фунт-силы')).toEqual('1 lbf');
		expect(insect('фунт-сил')).toEqual('1 lbf');
		expect(insect('Фунт-сил')).toEqual('1 lbf');	
    });

	it('new L(h.pound,["pounds","Pounds","pound","Pound","lb","Lb","фунтов","Фунтов","фунт","Фунт"])', function () {
		expect(insect('pounds')).toEqual('1 lb');
		expect(insect('Pounds')).toEqual('1 lb');
		expect(insect('pound')).toEqual('1 lb');
		expect(insect('Pound')).toEqual('1 lb');
		expect(insect('lb')).toEqual('1 lb');
		expect(insect('Lb')).toEqual('1 lb');
		expect(insect('фунтов')).toEqual('1 lb');
		expect(insect('Фунтов')).toEqual('1 lb');
		expect(insect('фунт')).toEqual('1 lb');
		expect(insect('Фунт')).toEqual('1 lb');
    });
	
	it('new L(ua.gallon,["gallons","Gallons","gallon","Gallon","gal","Gal","галлонов","Галлонов","галлон","Галлон"])', function () {
		expect(insect('gallons')).toEqual('1 gal');
		expect(insect('Gallons')).toEqual('1 gal');
		expect(insect('gallon')).toEqual('1 gal');
		expect(insect('Gallon')).toEqual('1 gal');
		expect(insect('gal')).toEqual('1 gal');
		expect(insect('Gal')).toEqual('1 gal');
		expect(insect('галлон')).toEqual('1 gal');
		expect(insect('Галлон')).toEqual('1 gal');
		expect(insect('галлонов')).toEqual('1 gal');
		expect(insect('Галлонов')).toEqual('1 gal');
    });
	
	it('new L(ua.pint,["pints","Pints","pint","Pint","пинта","Пинта","пинт","Пинт"])', function () {
		expect(insect('pints')).toEqual('1 pint');
		expect(insect('Pints')).toEqual('1 pint');
		expect(insect('pint')).toEqual('1 pint');
		expect(insect('Pint')).toEqual('1 pint');
		expect(insect('пинта')).toEqual('1 pint');
		expect(insect('Пинта')).toEqual('1 pint');
		expect(insect('пинт')).toEqual('1 pint');
		expect(insect('Пинт')).toEqual('1 pint');
    });
	
	it('new L(ua.cup,["cups","Cups","cup","Cup","чашек","Чашек","чашка","Чашка","стаканов","Стаканов","стакан","Стакан"])', function () {
		expect(insect('cups')).toEqual('1 cup');
		expect(insect('Cups')).toEqual('1 cup');
		expect(insect('cup')).toEqual('1 cup');
		expect(insect('Cup')).toEqual('1 cup');
		expect(insect('чашек')).toEqual('1 cup');
		expect(insect('Чашек')).toEqual('1 cup');
		expect(insect('чашка')).toEqual('1 cup');
		expect(insect('Чашка')).toEqual('1 cup');
		expect(insect('стаканов')).toEqual('1 cup');
		expect(insect('Стаканов')).toEqual('1 cup');
		expect(insect('стакан')).toEqual('1 cup');
		expect(insect('Стакан')).toEqual('1 cup');
    });
	
	it('new L(ua.tablespoon,["tablespoons","Tablespoons","tablespoon","Tablespoon","tbsp","Tbsp","ложек","Ложек","ложка","Ложка"])', function () {
		expect(insect('tablespoons')).toEqual('1 tablespoon');
		expect(insect('Tablespoons')).toEqual('1 tablespoon');
		expect(insect('tablespoon')).toEqual('1 tablespoon');
		expect(insect('Tablespoon')).toEqual('1 tablespoon');
		expect(insect('tbsp')).toEqual('1 tablespoon');
		expect(insect('Tbsp')).toEqual('1 tablespoon');
		expect(insect('ложек')).toEqual('1 tablespoon');
		expect(insect('Ложек')).toEqual('1 tablespoon');
		expect(insect('ложка')).toEqual('1 tablespoon');
		expect(insect('Ложка')).toEqual('1 tablespoon');
    });
	
	it('new L(ua.teaspoon,["teaspoons","Teaspoons","teaspoon","Teaspoon","tsp","Tsp","чайных ложек","Чайных ложек","чайная ложка","Чайная ложка"])', function () {
		expect(insect('teaspoons')).toEqual('1 teaspoon');
		expect(insect('Teaspoons')).toEqual('1 teaspoon');
		expect(insect('teaspoon')).toEqual('1 teaspoon');
		expect(insect('Teaspoon')).toEqual('1 teaspoon');
		expect(insect('tsp')).toEqual('1 teaspoon');
		expect(insect('Tsp')).toEqual('1 teaspoon');
		expect(insect('чайных ложек')).toEqual('1 teaspoon');
		expect(insect('Чайных ложек')).toEqual('1 teaspoon');
		expect(insect('чайная ложка')).toEqual('1 teaspoon');
		expect(insect('Чайная ложка')).toEqual('1 teaspoon');
    });
	
	it('new L(ua.fluidounce,["fluidounces","Fluidounces","fluidounce","Fluidounce","floz","Floz","жидких унций","Жидких унций","жидкая унция","Жидкая унция"])', function () {
		expect(insect('fluidounces')).toEqual('1 floz');
		expect(insect('Fluidounces')).toEqual('1 floz');
		expect(insect('fluidounce')).toEqual('1 floz');
		expect(insect('Fluidounce')).toEqual('1 floz');
		expect(insect('floz')).toEqual('1 floz');
		expect(insect('Floz')).toEqual('1 floz');
		expect(insect('жидких унций')).toEqual('1 floz');
		expect(insect('Жидких унций')).toEqual('1 floz');
		expect(insect('жидкая унция')).toEqual('1 floz');
		expect(insect('жидкая унция')).toEqual('1 floz');
    });
	
	it('new L(h.furlong,["furlong","Furlong","фурлонг","Фурлонг"])', function () {
		expect(insect('furlong')).toEqual('1 furlong');
		expect(insect('Furlong')).toEqual('1 furlong');
		expect(insect('фурлонг')).toEqual('1 furlong');
		expect(insect('Фурлонг')).toEqual('1 furlong');
    });
	
	it('new L(E.btu,["BTU","БТЕ"])', function () {
		expect(insect('BTU')).toEqual('1 BTU');
		expect(insect('БТЕ')).toEqual('1 BTU');
    });
	
	it('new L(E.psi,["psi","Psi"])', function () {
		expect(insect('psi')).toEqual('1 psi');
		expect(insect('Psi')).toEqual('1 psi');
    });
	
	it('new L(E.mmHg,["mmHg","мм рт.ст.","мм рт. ст."])', function () {
		expect(insect('mmHg')).toEqual('1 mmHg');
		//expect(insect('мм рт.ст.')).toEqual('1 mmHg');		//not working
		//expect(insect('мм рт. ст.')).toEqual('1 mmHg');		//not working
    });
	
	it('new L(ua.hogshead,["hogsheads","Hogsheads","hogshead","Hogshead","хогсхедов","Хогсхедов","хогсхед","Хогсхед","оксефтов","Оксефтов","оксефт","Оксефт"])', function () {
		expect(insect('hogsheads')).toEqual('1 hogshead');
		expect(insect('Hogsheads')).toEqual('1 hogshead');
		expect(insect('hogshead')).toEqual('1 hogshead');
		expect(insect('Hogshead')).toEqual('1 hogshead');
		expect(insect('хогсхедов')).toEqual('1 hogshead');
		expect(insect('Хогсхедов')).toEqual('1 hogshead');
		expect(insect('хогсхед')).toEqual('1 hogshead');
		expect(insect('Хогсхед')).toEqual('1 hogshead');
		expect(insect('оксефтов')).toEqual('1 hogshead');
		expect(insect('Оксефтов')).toEqual('1 hogshead');
		expect(insect('оксефт')).toEqual('1 hogshead');
		expect(insect('Оксефт')).toEqual('1 hogshead');
    });
	
	it('new L(ua.rod,["rods","Rods","rod","Rod","польей","Польей","поль","Поль","перчей","Перчей","перч","Перч"])', function () {
		expect(insect('rods')).toEqual('1 rod');
		expect(insect('Rods')).toEqual('1 rod');
		expect(insect('rod')).toEqual('1 rod');
		expect(insect('Rod')).toEqual('1 rod');
		expect(insect('польей')).toEqual('1 rod');
		expect(insect('Польей')).toEqual('1 rod');
		expect(insect('поль')).toEqual('1 rod');
		expect(insect('Поль')).toEqual('1 rod');
		expect(insect('перчей')).toEqual('1 rod');
		expect(insect('Перчей')).toEqual('1 rod');
		expect(insect('перч')).toEqual('1 rod');
		expect(insect('Перч')).toEqual('1 rod');
	});
	
	it('new L(P.divideUnits(E.pixel)(h.inch),["ppi","PPI","пикселей на дюйм","Пикселей на дюйм"])', function () {
		expect(insect('ppi')).toEqual('1 px/in');
		expect(insect('PPI')).toEqual('1 px/in');
		expect(insect('пикселей на дюйм')).toEqual('1 px/in');
		expect(insect('Пикселей на дюйм')).toEqual('1 px/in');
    });
	
	it('new L(P.divideUnits(E.dot)(h.inch),["dpi","DPI","точек на дюйм","Точек на дюйм"])', function () {
		expect(insect('dpi')).toEqual('1 dot/in');
		expect(insect('DPI')).toEqual('1 dot/in');
		//expect(insect('точек на дюйм')).toEqual('1 dot/in');		//not working
		//expect(insect('Точек на дюйм')).toEqual('1 dot/in');		//not working
    });
	
	it('new L(E.piece,["pieces","Pieces","piece","Piece","штука","Штука","штук","Штук","шт.","Шт.","шт","Шт"])', function () {
		expect(insect('pieces')).toEqual('1 piece');
		expect(insect('Pieces')).toEqual('1 piece');
		expect(insect('piece')).toEqual('1 piece');
		expect(insect('Piece')).toEqual('1 piece');
		expect(insect('штук')).toEqual('1 piece');
		expect(insect('Штук')).toEqual('1 piece');
		expect(insect('штука')).toEqual('1 piece');
		expect(insect('Штука')).toEqual('1 piece');
		expect(insect('шт.')).toEqual('1 piece');
		expect(insect('Шт.')).toEqual('1 piece');
		expect(insect('шт')).toEqual('1 piece');
		expect(insect('Шт')).toEqual('1 piece');
    });
	
	it('new L(E.person,["persons","Persons","person","Person","people","People","человека","Человека","человек","Человек","чел.","Чел.","чел","Чел"])', function () {
		expect(insect('persons')).toEqual('1 person');
		expect(insect('Persons')).toEqual('1 person');
		expect(insect('person')).toEqual('1 person');
		expect(insect('Person')).toEqual('1 person');
		expect(insect('people')).toEqual('1 person');
		expect(insect('People')).toEqual('1 person');
		expect(insect('человека')).toEqual('1 person');
		expect(insect('Человека')).toEqual('1 person');
		expect(insect('человек')).toEqual('1 person');
		expect(insect('Человек')).toEqual('1 person');
		expect(insect('чел.')).toEqual('1 person');
		expect(insect('Чел.')).toEqual('1 person');
		expect(insect('чел')).toEqual('1 person');
		expect(insect('Чел')).toEqual('1 person');
    });
	
	it('new L(A.dollar,["dollars","Dollars","dollar","Dollar","USD","usd","$","долларов","Долларов","доллар","Доллар","дол.","дол","у.е."])', function () {
		expect(insect('dollars')).toEqual('1 USD');
		expect(insect('Dollars')).toEqual('1 USD');
		expect(insect('dollar')).toEqual('1 USD');
		expect(insect('Dollar')).toEqual('1 USD');
		expect(insect('USD')).toEqual('1 USD');
		expect(insect('usd')).toEqual('1 USD');
		expect(insect('$')).toEqual('1 USD');
		expect(insect('долларов')).toEqual('1 USD');
		expect(insect('Долларов')).toEqual('1 USD');
		expect(insect('доллар')).toEqual('1 USD');
		expect(insect('Доллар')).toEqual('1 USD');
		expect(insect('дол.')).toEqual('1 USD');
		expect(insect('дол')).toEqual('1 USD');
		expect(insect('у.е.')).toEqual('1 USD');
    });
	
	it('new L(A.euro,["euros","Euros","euro","Euro","EUR","\u20ac","евро","Eвро"])', function () {
		expect(insect('euros')).toEqual('1 EUR');
		expect(insect('Euros')).toEqual('1 EUR');
		expect(insect('euro')).toEqual('1 EUR');
		expect(insect('Euro')).toEqual('1 EUR');
		expect(insect('EUR')).toEqual('1 EUR');
		expect(insect('\u20ac')).toEqual('1 EUR');
		expect(insect('евро')).toEqual('1 EUR');
		expect(insect('Eвро')).toEqual('1 EUR');
    });
	
	//commented because not added
	//it('new L(A.bitcoin,["bitcoins","Bitcoins","bitcoin","Bitcoin","BTC","btc","\u20BF","биткоинов","биткоин"])	//just leave this here, if you want to add bitcoin.', function () {
	//	expect(insect('bitcoins')).toEqual('1 BTC');
	//	expect(insect('Bitcoins')).toEqual('1 BTC');
	//	expect(insect('bitcoin')).toEqual('1 BTC');
	//	expect(insect('Bitcoin')).toEqual('1 BTC');
	//	expect(insect('BTC')).toEqual('1 BTC');
	//	expect(insect('btc')).toEqual('1 BTC');
	//	expect(insect('\u20BF')).toEqual('1 BTC');
	//	expect(insect('биткоинов')).toEqual('1 BTC');
	//	expect(insect('биткоин')).toEqual('1 BTC');
    //});
});

describe("Insect-test 4.", function () {
	it("test1 - conversion commands("+'->'+', '+'\u2192'+', '+'\u279e'+', '+'to'+")", function () {
		expect(insect('5 kJ -> kcal')).toEqual('1.19503 kcal');
		expect(insect('5 kJ \u2192 kcal')).toEqual('1.19503 kcal');
		expect(insect('5 kJ \u279e kcal')).toEqual('1.19503 kcal');
		expect(insect('5 kJ to kcal')).toEqual('1.19503 kcal');
    });
	
	it("Some convertations:", function () {
		expect(insect('840 ПВтч -> килокалории')).toEqual('722753000000000000 kcal');
		expect(insect('(1800 W * 24 часа) -> megaджоулей')).toEqual('155.52 MJ');
	});
	
	
});

//describe("empty commented test to copy", function () {
//	it('test1', function () {
//		expect(insect('input')).toEqual('output');
//    });
//});



