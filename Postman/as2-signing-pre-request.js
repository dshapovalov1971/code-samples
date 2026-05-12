/*
 * Required Postman variables:
 *   as2_from
 *   as2_to
 *   as2_signing_p12_base64
 *   as2_signing_p12_password
 *
 * Put the unsigned EDI in the request raw body. This script replaces it with
 * a signed AS2 multipart/signed MIME body at send time.
 */

const FORGE_URL = 'https://cdn.jsdelivr.net/npm/node-forge@latest/dist/forge.min.js';
const CONTENT_TYPE = 'application/edi-x12';
const FILENAME = 'message.edi';
const SIGNING_ALGORITHM = 'sha-256';
const MESSAGE_ID_DOMAIN = 'postman.local';

pm.sendRequest(FORGE_URL, (err, response) => {
  const error = message => {throw new Error(message);};
  if (err) error(err.message || err);
  const requiredVariable = name => pm.variables.get(name) || error(`Missing required Postman variable: ${name}`);
  const foldBase64 = base64 => base64.replace(/(.{76})/g, '$1\r\n').replace(/\r\n$/g, '');
  const randomHex = bytes => CryptoJS.lib.WordArray.random(bytes).toString(CryptoJS.enc.Hex);
  const window = {};
  eval(response.text());
  const forge = window.forge;
  const p12 = forge.pkcs12.pkcs12FromAsn1(forge.asn1.fromDer(forge.util.decode64(requiredVariable('as2_signing_p12_base64').replace(/\s+/g, ''))), false, requiredVariable('as2_signing_p12_password'));
  const bags = type => p12.getBags({bagType: forge.pki.oids[type]})[forge.pki.oids[type]] || [];
  const id = {
    key: bags('pkcs8ShroudedKeyBag').concat(bags('keyBag'))[0].key,
    certificate: bags('certBag')[0].cert
  };
  const boundary = '----PostmanAS2-' + randomHex(12);
  const contentPart = [
    'Content-Type: ' + CONTENT_TYPE + '; name=' + FILENAME,
    'Content-Transfer-Encoding: base64',
    'Content-Disposition: attachment; filename=' + FILENAME,
    '',
    foldBase64(CryptoJS.enc.Base64.stringify(CryptoJS.enc.Utf8.parse((pm.request.body.raw || '').replace(/\r\n|\r|\n/g, '\r\n'))))
  ].join('\r\n');
  const p7 = forge.pkcs7.createSignedData();

  p7.content = forge.util.createBuffer(contentPart, 'binary');
  p7.addCertificate(id.certificate);
  p7.addSigner({
    key: id.key,
    certificate: id.certificate,
    digestAlgorithm: forge.pki.oids.sha256,
    authenticatedAttributes: [
      {type: forge.pki.oids.contentType, value: forge.pki.oids.data},
      {type: forge.pki.oids.messageDigest},
      {type: forge.pki.oids.signingTime, value: new Date()}
    ]
  });
  p7.sign({detached: true});

  pm.request.body.update({
    mode: 'raw',
    raw: [
      '--' + boundary,
      contentPart,
      '--' + boundary,
      'Content-Type: application/pkcs7-signature; name=smime.p7s',
      'Content-Disposition: attachment; filename=smime.p7s',
      'Content-Transfer-Encoding: base64',
      '',
      foldBase64(forge.util.encode64(forge.asn1.toDer(p7.toAsn1()).getBytes())),
      '--' + boundary + '--',
      ''
    ].join('\r\n'),
    options: {raw: {language: 'text'}}
  });

  Object.entries({
    'AS2-From': requiredVariable('as2_from'),
    'AS2-To': requiredVariable('as2_to'),
    'Message-ID': '<' + randomHex(4) + '-' + randomHex(2) + '-' + randomHex(2) + '-' + randomHex(2) + '-' + randomHex(6) + '@' + MESSAGE_ID_DOMAIN + '>',
    'Content-Type': 'multipart/signed; protocol="application/pkcs7-signature"; micalg=' + SIGNING_ALGORITHM + '; boundary="' + boundary + '"'
  }).forEach(header => pm.request.headers.upsert({key: header[0], value: header[1]}));
});
