
// utilities
var leP = function (end)   { return function (x) { return x <= end;   }; };
var geP = function (start) { return function (x) { return x >= start; }; };

var inRangeP = function (start, end) {
    return function (x) { 
        return geP(start)(x) && leP(end)(x);
    };
};
var S = function (accessorChain) {
    return function (val) {
        return function (trg) {
            trg = $(trg);
            
            var acLen = accessorChain.length;
            var i = -1;
            
            while (++i < acLen - 1)
                trg = trg[accessorChain[i]];
            
            trg[accessorChain[i]] = val;
        };
    };
};

var C = function (f, g) {
    return function (x) {
        return f(g(x));
    };
};

var T = function (f) {
    return function (x) {
        return function () {
            return f(x);
        };
    };
};

// domain
var convert = function (srcRadixS, trgRadixS, srcNumS) {
    var srcRadix = parseInt(srcRadixS);
    var trgRadix = parseInt(trgRadixS);
    var srcNum   = parseInt(srcNumS, srcRadix);
    
    return srcNum.toString(trgRadix);
};

var validateB = function (trg, pred, okT, errorT) {
    extractValueB(trg, 'value')    
        .liftB(pred)
        .liftB(function (p) {
            if (p)
                okT();
            else
                errorT();
        });
};

// control
var initNSC = function () {
    $('convert').onclick = function () {
        $('trgNum').value = convert (
            $('srcRadix').value,
            $('trgRadix').value,
            $('srcNum').value
        );
    };
    
    var setDisabled = S(['disabled']);
    var setBGColor  = S(['style', 'backgroundColor']);
    
    var mkOkT = T(C(
        T(setDisabled(false))('convert'),
        setBGColor('white')
    ));
    
    var mkErrorT = T(C(
        T(setDisabled(true))('convert'),
        setBGColor('red')
    ));
    
    ['srcRadix', 'trgRadix'].forEach(function (trg) {
        validateB(
            trg, C(inRangeP(1, 16), parseInt),
            mkOkT(trg), mkErrorT(trg)
        );
    });
    
    validateB(
        'srcNum', C(geP(0), parseInt),
        mkOkT('srcNum'), mkErrorT('srcNum')
    );
};