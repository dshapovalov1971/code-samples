const path = require('path');
const fs = require('fs');

const inPathExp = 'src/components/.exported_teleporthq/groups wip-react/src/views';
const inManualFile = 'src/i18n/enUS.manual.json';
const outFile = 'src/i18n/enUS.json';
const o = {exported: {}};
fs.readdirSync(inPathExp).filter(f => path.extname(f) === '.js').forEach(f => {
	fs.readFileSync(`${inPathExp}/${f}`, 'utf8').match(/(?<=<span>).+(?=<\/span>)/g).forEach(v => {
		o.exported[v] = v;
	});
});
fs.writeFileSync(outFile, JSON.stringify({...JSON.parse(fs.readFileSync(inManualFile, 'utf8')), ...o}, null, '\t'));
console.log('Localization object generated');
