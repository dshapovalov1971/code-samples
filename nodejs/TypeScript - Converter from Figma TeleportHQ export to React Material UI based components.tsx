/*
  How to use this converter:
    - step 1: Create free account at teleporthq.io
    - step 2: Delete all TeleportHQ projects if they were previously created
    - step 3: In Figma select components you want to export and use TeleportHQ plugin to export them
    - step 4: In created TeleportHQ project export source code as React app, it will created ZIP file with React ready source files
    - step 5: Unzip exported ZIP file into src folder of your project, overriding any source code which was exported before
    - step 6: In your React component render() method call function from exported component source and return the result. This will render your React component exactly to match Figma design for selected component
    - step 7: In your React component render() method wrap call to exported component function into provided here transformExportedDesign() method, passing element map as a first parameter and 
    		result from exported function as a second parameter. Use provided below default elementMap constant as an example. Each exported control can be mapped to your specific JSX element,
      		'remove' string, which will remove mapped control keeping its children, and 'removeAll' string, which will remove mapped control toghether with its children. This will allow tayloring
		exported markup to the specific functionality and component library of your React component. This is implemented to use Material UI library, but could be used for other libraries as well
    - step 8: In case of updates in Figmea design, just repeat steps 2-5 to propagate updates into your app  
*/
import {FormControl, Box, Typography, Button, InputLabel, Select, Skeleton, MenuItem, Tabs, Tab, type TypographyTypeMap, Dialog} from '@mui/material';
import {PureComponent, type HTMLAttributes, type ImgHTMLAttributes} from 'react';
import React from 'react';
import {Translation} from 'react-i18next';
import exportedAssets from './bundle-loader';

type ElementMap = Record<string, JSX.Element | 'remove' | 'removeAll'>;
const elementMap: ElementMap = {
	'frame-button': <Button />,
	text: <Typography />,
	tabs: <Tabs />,
	tab: <Tab />,
	modal: <Dialog open={false} />,
	'modal-window': 'remove',
	'modal-surface': 'remove',
	header: 'removeAll',
	footer: 'removeAll',
	'framebase-scrollable-tab-text-elements': 'remove',
};

export const transformExportedDesign = (cfg: ElementMap, el: JSX.Element) => {
	const prefix = (el.props as HTMLAttributes<HTMLElement>).className!.slice(0, -'container'.length);
	const parseClasses = (className?: string) => {
		const classes = className?.split(' ') ?? [];
		const elName = classes.length ? classes[0].substring(prefix.length) : '';
		return {
			elName,
			elType: elName.replace(/(.*)(?<=\D)\d*/, '$1'),
			typographyVariant: classes.length > 1 ? classes[1] : '',
		};
	};

	const processTree = (e: JSX.Element, p: JSX.Element): JSX.Element[] => {
		let props = {};
		let children = e.props.children as JSX.Element[];
		if (e.type === 'img') {
			(props as ImgHTMLAttributes<HTMLImageElement>).src
				= exportedAssets(`.${(e.props as ImgHTMLAttributes<HTMLImageElement>).src!.substring('/playground_assets'.length)}`) as string;
		}

		const classes = parseClasses(e.props.className);
		let cfgElement = cfg[classes.elName];
		const foundByName = Boolean(cfgElement);
		cfgElement = cfgElement ?? elementMap[classes.elType];
		const replaceElement = React.isValidElement(cfgElement);
		cfgElement = cfgElement ?? e;
		if (replaceElement) {
			(props as HTMLAttributes<HTMLElement>).className = (e.props as HTMLAttributes<HTMLElement>).className;
			const typedElement = cfgElement as JSX.Element;
			children = typedElement.props.children || (foundByName && typedElement.type === Typography)
				? typedElement.props.children as JSX.Element[] : children;
			switch (typedElement.type) {
				case Button: {
					(props as HTMLAttributes<HTMLElement>).className = [
						...(e.props.children.props.children.props as HTMLAttributes<HTMLElement>).className!.split(' '),
						...(e.props.children.props as HTMLAttributes<HTMLElement>).className!.split(' '),
						...(props as HTMLAttributes<HTMLElement>).className!.split(' '),
					].join(' ');
					//
					// if (!((cfgElement as JSX.Element).props as {startIcon: JSX.Element}).startIcon) {
					// 	const icon = [((e.props.children as JSX.Element).props.children as JSX.Element).props.children as JSX.Element[]]
					// 		.flatMap(m => m).find(c => c.type === 'img');
					// 	if (icon) {
					// 		const imgProps = icon.props as ImgHTMLAttributes<HTMLImageElement>;
					// 		(props as {startIcon: JSX.Element}).startIcon = React.cloneElement(icon, {src: exportedAssets(`.${imgProps.src!.substring('/playground_assets'.length)}`) as string});
					// 	}
					// }

					children = [((e.props.children as JSX.Element).props.children as JSX.Element).props.children as JSX.Element[]]
						.flatMap(m => m).find(c => parseClasses(c.props.className).elType === 'frametextbox')?.props.children as JSX.Element[];
					break;
				}

				case Typography: {
					cfgElement = <Box />;
					props = {...props, ...typedElement.props as Record<string, unknown>};
					delete (props as HTMLAttributes<HTMLElement>).children;
					const te = children as unknown as JSX.Element;
					children = children ?? '‏';
					children = [React.createElement(typedElement.type, {variant: classes.typographyVariant}, React.isValidElement(children) ? React
						.cloneElement(te, {}, <Translation>{t => t`exported.${te.props.children as string}`}</Translation>) : children)];
					break;
				}

				case SelectW:
					cfgElement = <Box />;
					children = [typedElement];
					break;
				case Tab: {
					const te = (e.props.children as JSX.Element[])
						.find(c => parseClasses((c.props as HTMLAttributes<HTMLElement>).className).elType === 'tab-content')!
						.props as HTMLAttributes<HTMLElement>;
					(props as HTMLAttributes<HTMLElement>).className = [
						...(p.props as HTMLAttributes<HTMLElement>).className!.split(' '),
						...(props as HTMLAttributes<HTMLElement>).className!.split(' '),
						...te.className!.split(' '),
					].join(' ');
					props = {
						...props,
						label: React.createElement(Typography, {
							variant: parseClasses(((te.children as JSX.Element).props as HTMLAttributes<HTMLElement>).className)
								.typographyVariant as TypographyTypeMap['props']['variant'],
						}, <Translation>{t => t`exported.${((te.children as JSX.Element).props.children as JSX.Element).props.children as string}`}</Translation>),
					};
					children = [];
					break;
				}

				case Dialog:
					if (p.type === Dialog) {
						cfgElement = 'remove';
					} else {
						(props as HTMLAttributes<HTMLElement>).className = '';
						(props as {PaperProps: HTMLAttributes<HTMLElement>}).PaperProps = {
							...{className: ((e.props.children as JSX.Element[])
								.find(c => parseClasses((c.props as HTMLAttributes<HTMLElement>).className).elType === 'modal')!
								.props as HTMLAttributes<HTMLElement>).className},
							...((cfgElement as JSX.Element).props as {PaperProps: HTMLAttributes<HTMLElement>}).PaperProps,
						};
					}

					break;
				case Skeleton:
					children = [];
					break;
				default:
			}
		}

		const parentElement = React.isValidElement(cfgElement) ? cfgElement : e;
		const processChildren = (): JSX.Element[] => Array.isArray(children)
			? children.flatMap(v => processTree(v, parentElement))
			: React.isValidElement(children) ? processTree(children, parentElement) : [children];
		return cfgElement === 'removeAll' ? []
			: cfgElement === 'remove' ? processChildren()
				: [React.cloneElement(cfgElement, props, ...processChildren())];
	};

	return processTree(el.props.children[1], el)[0];
};

export class SelectW extends PureComponent<{
	disabled?: boolean;
	skeleton?: boolean;
	label: string;
	value: () => unknown;
	onChange: (v: unknown) => void;
	items?: Array<{
		value: unknown;
		label: string;
	}>;
}> {
	render() {
		const uniqueId = (Date.now() * Math.random()).toString();
		return <FormControl disabled={this.props.disabled}>
			{this.props.skeleton ? <Skeleton height={60} /> : <>
				<InputLabel id={uniqueId}>{this.props.label}</InputLabel>
				<Select
					labelId={uniqueId}
					value={this.props.value()}
					onChange={e => {
						this.props.onChange(e.target.value);
					}}
				>
					{this.props.items?.map(c =>
						<MenuItem key={c.value as string} value={c.value as string}>{c.label}</MenuItem>,
					)}
				</Select>
			</>}
		</FormControl>;
	}
}
