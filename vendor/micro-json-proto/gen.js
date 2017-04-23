"use strict";
let program    = require('commander');
let YAML       = require('yamljs');
let Handlebars = require('handlebars');
let fs         = require('fs');
let Promise    = require('bluebird');
let readFile   = Promise.promisify(fs.readFile);
let writeFile  = Promise.promisify(fs.writeFile);
let path       = require('path');


Handlebars.registerHelper('lowerFirst', str => str.length > 0 ? str[0].toLowerCase() + str.substr(1) : str);
Handlebars.registerHelper('isListType', (v, options) => Array.isArray(v) ? options.fn(this) : options.inverse(this));
Handlebars.registerHelper('listBaseType', v => { console.log(v); return v[0]});
Handlebars.registerHelper('json', v => JSON.stringify(v));

Handlebars.registerHelper('length', v => v.length);
Handlebars.registerHelper('jsonMapFn', (v) => {
    let len = Object.keys(v).length;
    if (len > 8)
        throw new Error("elm Json.Decode.map supports only up to 8 fields per object");

    return `Json.Decode.map${len}`

});

Handlebars.registerHelper('ifCond', function (v1, operator, v2, options) {
    switch (operator) {
        case '==':
            return (v1 === v2) ? options.fn(this) : options.inverse(this);
        case '===':
            return (v1 === v2) ? options.fn(this) : options.inverse(this);
        case '!=':
            return (v1 !== v2) ? options.fn(this) : options.inverse(this);
        case '!==':
            return (v1 !== v2) ? options.fn(this) : options.inverse(this);
        case '<':
            return (v1 < v2) ? options.fn(this) : options.inverse(this);
        case '<=':
            return (v1 <= v2) ? options.fn(this) : options.inverse(this);
        case '>':
            return (v1 > v2) ? options.fn(this) : options.inverse(this);
        case '>=':
            return (v1 >= v2) ? options.fn(this) : options.inverse(this);
        case '&&':
            return (v1 && v2) ? options.fn(this) : options.inverse(this);
        case '||':
            return (v1 || v2) ? options.fn(this) : options.inverse(this);
        default:
            return options.inverse(this);
    }
});

// generateOperationKindData("src/SEd/Operations/OperationKindData.yaml")


program
    .version('0.0.1')
    .usage('[options] <file ...>')
    .option("-l, --lang <template_file>", "Set the template file to use")
    .option("-o, --output <output_file>", "Set the output directory", '')
    .parse(process.argv);

if (!program.lang)
    throw new Error('--lang required');

if (!program.output)
    throw new Error('--output required');

loadTemplate(program.lang)
    .then(tpl => Promise.all(program.args.map(file => generateOperationKindData(program.lang, tpl, file))))
    .then(console.log)
    .catch(console.error);

// program.args.forEach(file => generateOperationKindData(file));

function getOutputName(tplName, inFile) {
    return path.join(program.output, path.basename(inFile, '.yaml') + '.' + tplName);
}

function loadTemplate(name) {
    let path = __dirname + `/templates/${name}.handlebars`;
    return readFile(path, 'utf-8')
        .then(Handlebars.compile);
}


function generateOperationKindData(tplName, tpl, inFile) {
    let dataIn = YAML.load(inFile);
    let expand = d => expandYaml(dataIn.lang[program.lang], d);
    // get the package from the lang
    let data = {
        "package": expand("$$package"),
        "types": expand(dataIn.types)
    };

    let path = program.output;
    return writeFile(path, tpl(data), "utf-8")
        .then(_ => { return { lang: tplName, source: inFile, destination: path }; });
}



function expandYaml(lang, types) {
    let recur = ts => expandYaml(lang, ts);
    if (typeof types === 'string') {
        if (types.substr(0,2) === '$$') {
            let key = types.substr(2);
            let val = lang[key];
            if (val) {
                return val;
            } else {
                throw new Error(`Cannot find token value for ${key} in lang.`)
            }
        } else {
            return types;
        }
    } else if (Array.isArray(types)) {
        return types.map(child => recur(child));
    } else {
        let o = {};
        Object.keys(types).forEach(k => o[k] = recur(types[k]));
        return o;
    }
}
