const https = require('https');
const fs = require('fs');
const path = require('path');

const gigyaAPICommonParams = {
	apiKey: process.env.REACT_APP_CDC_API_KEY,
	userKey: process.env.REACT_APP_CDC_APP_KEY,
	secret: process.env.REACT_APP_CDC_APP_SECRET,
};

const output = [];
new Promise((s, e) => {
	const req = https.request({
		hostname: `accounts.${process.env.REACT_APP_CDC_DC}`,
		path: '/accounts.getSchema',
		method: 'POST',
		headers: {
			'Content-Type': 'application/x-www-form-urlencoded',
		},
	}, o => {
		let r = '';
		o.on('data', d => {
			r += d.toString();
		}).on('end', () => {
			r = JSON.parse(r);
			(r.errorCode ? e : s)(r);
		});
	}).on('error', e);
	req.write(Object.entries(gigyaAPICommonParams).map(([k, v]) => `${k}=${encodeURIComponent(v)}`).join('&'));
	req.end();
}).then(r => {
	output.push('export type GigyaUserRecord = {');
	Object.entries(r).filter(([k]) => k.endsWith('Schema')).forEach(([k, v]) => {
		processFields(v.fields, k, ['profileSchema', 'dataSchema'].indexOf(k) >= 0, 1);
	});

	output.push('};');
	output.push('');

	const groupModels = process.env.REACT_APP_GROUP_MODELS.split(',');
	output.push('export enum GigyaGroupModels {');
	groupModels.forEach(e => {
		output.push(`\t${e} = '${e}',`);
	});
	output.push('}');
	output.push('');

	return Promise.all(groupModels.map(m => new Promise((s, e) => {
		const req = https.request({
			hostname: `accounts.${process.env.REACT_APP_CDC_DC}`,
			path: '/accounts.groups.getSchema',
			method: 'POST',
			headers: {
				'Content-Type': 'application/x-www-form-urlencoded',
			},
		}, o => {
			let r = '';
			o.on('data', d => {
				r += d.toString();
			}).on('end', () => {
				r = JSON.parse(r);
				(r.errorCode ? e : s)(r);
			});
		}).on('error', e);
		req.write(Object.entries({
			...gigyaAPICommonParams,
			model: m,
		}).map(([k, v]) => `${k}=${encodeURIComponent(v)}`).join('&'));
		req.end();
	})));
}).then(r => {
	output.push('export type GigyaGroupSchemas = {');
	r.forEach(m => {
		output.push(`\t${m.model}: {`);
		Object.entries(m.schema).forEach(([k, v]) => {
			processFields(v, k, false, 2);
		});

		output.push('\t};');
	});
	output.push('};');
	output.push('');

	const outputFile = process.argv.slice(-1)[0];
	const dir = path.dirname(outputFile);
	if (!fs.existsSync(dir)) {
		fs.mkdirSync(dir);
	}

	fs.writeFileSync(outputFile, output.join('\n'));
	console.log('Gigya schema definitions generated');
}).catch(e => {
	console.log(e);
	throw e;
});

function processFields(r, p, canBeArray, indent) {
	if (Object.getOwnPropertyNames(r).length) {
		const padding = '\t'.repeat(indent);
		output.push(`${padding}${p}: {`);
		const str = {};
		Object.entries(r).forEach(([k, v]) => {
			const path = k.split('.');
			path.slice(0, -1).reduce((p, c) => {
				p[c] = p[c] ?? {};
				return p[c];
			}, str)[path.slice(-1)[0]] = {__meta: v};
		});

		const outputType = (indent, o) => {
			const padding = '\t'.repeat(indent);
			Object.entries(o).forEach(([k, v]) => {
				if (v.__meta) {
					output.push(`${padding}${k}${v.__meta.required ? '' : '?'}: ${mapType(v.__meta.type)}${canBeArray ? ` | ${mapType(v.__meta.type)}[]` : ''};`);
				} else {
					output.push(`${padding}${k}: {`);
					outputType(indent + 1, v);
					output.push(`${padding}}${canBeArray ? ' | Array<{' : ';'}`);
					if (canBeArray) {
						outputType(indent + 1, v);
						output.push(`${padding}}>;`);
					}
				}
			});
		};

		outputType(indent + 1, str);
		output.push(`${padding}};`);
	}
}

function mapType(type) {
	return {
		long: 'number',
		'basic-string': 'string',
		float: 'number',
		date: 'Date',
		consent: 'undefined',
	}[type] ?? type;
}
