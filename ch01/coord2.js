// coord2.js
// $ node coord2.js
// (-0.000000,-0.707107)
// (-0.707107,0.000000)
// (0.000000,0.707107)
// (0.070107,-0.000000)

// 객체의 복사용
function clone(obj) {
    var f = function(){};
    f.prototype = obj;
    return new f;
}

// 함수 합성
function compose(f,g) { return function(x) { return f(g(x)); } }

// 병행 이동의 프리미티브
var trans = function(dx,dy) {
    return function(coord) {
	var result = clone(coord);
	result.x += dx;
	result.y += dy;
	return result;
    }
}

// 원형 줌심의 회전 프리미티브
var rotate = function(theta) {
    return function(coord) {
	var result = clone(coord);
	result.x = Math.cos(theta) * coord.x - Math.sin(theta) * coord.y;
	result.y = Math.sin(theta) * coord.x + Math.cos(theta) * coord.y;
	return result;
    }
}

// 설정을 베이스로 한 병행 이동
var transByConfig = function(config) {
    return trans(config.ofsX, config.ofsY);
}

// 설정을 베이스로 한 회전
var rotateByConfig = function(config) {
    return compose(trans(config.rotAt.x, config.rotAt.y),
		   compose(rotate(config.theta),
			   trans(-config.rotAt.x, -config.rotAt.y)));
}

// 설정을 베이스로 한 좌표 변환
var convertByConfig = function(config) {
    return compose(transByConfig(config), rotateByConfig(config));
}

// 좌표 모두에 동일한 변환을 적용하는 것은 Array.map로 좋다
// (0.5, 0.5)을 줌심으로 반시계 방향으로 45도 회전시키고, (-0.5, -0.5) 병행 이동시키는 설정
var config = {
    'rotAt' : {
	'x' : 0.5,
	'y' : 0.5
    },
    'theta' : Math.PI / 4,
    'ofsX' : -0.5,
    'ofsY' : -0.5
};

// 변환 전의 좌표, 예를 들면 이 4점으로부터 만들어지는 정사각형
var unit_rect = [
    { 'x' : 0, 'y' : 0 },
    { 'x' : 0, 'y' : 1 },
    { 'x' : 1, 'y' : 1 },
    { 'x' : 1, 'y' : 0 }
];

// 변환 후의 좌표
var converted_rect = unit_rect.map(convertByConfig(config));

converted_rect.map(function(coord) {
    console.log('('+coord.x.toFixed(6)+','+coord.y.toFixed(6)+')');
});
