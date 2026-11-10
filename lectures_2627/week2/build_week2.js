const pptxgen = require('pptxgenjs');
const fs=require('fs'), path=require('path'), cp=require('child_process'), crypto=require('crypto');
const {latexToSvgDataUri}=require('../week1/pptxgenjs_helpers/latex');
const {svgToDataUri}=require('../week1/pptxgenjs_helpers/svg');
const {imageSizingContain}=require('../week1/pptxgenjs_helpers/image');
const {warnIfSlideHasOverlaps,warnIfSlideElementsOutOfBounds}=require('../week1/pptxgenjs_helpers/layout');
const {autoFontSize}=require('../week1/pptxgenjs_helpers/text');
const {COLORS, FONT_FAMILIES}=require('../../slides-common/js/blr_theme');
const primitives=require('../../slides-common/js/pptx_layout_primitives');
const {writePresentation}=require('../../slides-common/js/write_presentation');
const {addStudyOverviewSlide,addConditionalModelSlide}=require('../../slides-common/js/course_builders');
const {defineTemplateMasters,addBase,addSlideTitle,addTextBox,addAssetFrame,addNoteBox,addBrandedTitleSlide,addSectionSlide,addBulletVisualSlide,addComparisonSlide,addFigureSlide}=primitives;
const pptx=new pptxgen(); pptx.layout='LAYOUT_WIDE'; pptx.author='Beyond MLR';pptx.subject='Week 2 starter: cluster-randomized trials with a common treatment effect';pptx.title='Beyond MLR | Week 2';pptx.lang='en-GB';pptx.theme={headFontFace:FONT_FAMILIES.sans,bodyFontFace:FONT_FAMILIES.sans,lang:'en-GB'};defineTemplateMasters(pptx);
const C={navy:COLORS.primary,teal:COLORS.primary,orange:COLORS.accent,ink:COLORS.text,muted:COLORS.muted,pale:COLORS.surfaceMuted,line:COLORS.cardLine,white:COLORS.card,gray:COLORS.background};
const W=13.333,H=7.5;
const R=JSON.parse(fs.readFileSync(path.join('assets','model_results.json'),'utf8'));
const fmt=(v,n=1)=>Number(v).toFixed(n), ci=v=>`${fmt(v.lower)} to ${fmt(v.upper)}`;
function txt(s,t,x,y,w,h,o={}){const face=o.fontFace||FONT_FAMILIES.sans, size=o.fontSize||20; const measured=autoFontSize(t,face,{x,y,w,h,fontSize:size,minFontSize:Math.max(8,size-1),maxFontSize:size,mode:'shrink',margin:0});s.addText(t,{...measured,color:C.ink,breakLine:false,valign:'mid',...o});}
function line(s,x1,y1,x2,y2,color=C.line,width=1){s.addShape(pptx.ShapeType.line,{x:x1,y:y1,w:x2-x1,h:y2-y1,line:{color,width}})}
function header(s,title,section='WEEK 2'){addBase(s);addSlideTitle(s,title);}
function footer(s,n){txt(s,'BEYOND MLR · WEEK 2',10.15,7.18,2.1,.16,{fontSize:8,color:C.muted,align:'right'});txt(s,String(n),12.35,7.18,.35,.16,{fontSize:8,color:C.muted,align:'right'});}
function slide(title,section){let s=pptx.addSlide();header(s,title,section);return s;}
function bullet(s,items,x,y,w,fs=20,dy=.61){items.forEach((t,i)=>{txt(s,'–',x,y+i*dy,.2,.31,{fontSize:fs,color:C.teal});txt(s,t,x+.31,y+i*dy,w-.31,dy-.12,{fontSize:fs,valign:'top',breakLine:false});});}
function fig(s,file,x,y,w,h){addAssetFrame(s,{box:{x,y,w,h},assetPath:path.join('assets',file),fit:'contain'});}
function panelText(s,label,body,x,y,w){addTextBox(s,{box:{x:x-.13,y:y-.14,w:w+.2,h:1.78},title:label,text:body,fontSize:19,minFontSize:17});}
function equation(s,tex,x,y,w,h){
  const hash=crypto.createHash('sha1').update(tex).digest('hex').slice(0,12);
  const dir=path.join('assets','equations');fs.mkdirSync(dir,{recursive:true});
  const data=latexToSvgDataUri(tex); // LaTeX-to-SVG via the bundled helper.
  const svgPath=path.join(dir,'mathjax_'+hash+'.svg');
  fs.writeFileSync(svgPath,Buffer.from(data.split(',')[1],'base64'));
  const pngPath=path.join(dir,'eq_'+hash+'.png');
  if(!fs.existsSync(pngPath)) throw new Error('Missing LaTeX PNG fallback: '+pngPath);
  s.addImage({path:pngPath,...imageSizingContain(pngPath,x,y,w,h)});
}

function table(s,rows,x,y,widths,rowH=.5){let yy=y;rows.forEach((row,r)=>{const sw=widths.reduce((a,b)=>a+b,0);/* Row background intentionally contains table text. */ s.addShape(pptx.ShapeType.rect,{x,y:yy,w:sw,h:rowH,line:{color:r?C.line:C.navy,width:.35},fill:{color:r?(r%2?C.white:'E6F0ED'):C.navy}});let xx=x;row.forEach((v,c)=>{txt(s,String(v),xx+.1,yy+.04,widths[c]-.2,rowH-.08,{fontSize:r?17:14,bold:r===0,color:r===0?C.white:C.ink,align:c?'center':'left'});xx+=widths[c];});yy+=rowH;});}
function takeaway(s,text,x,y,w,h=.48){/* Filled panel intentionally contains the takeaway text. */ s.addShape(pptx.ShapeType.roundRect,{x,y,w,h,rectRadius:.06,line:{color:C.navy,width:.3},fill:{color:C.navy}});txt(s,text,x+.18,y+.04,w-.36,h-.08,{fontSize:17,bold:true,color:C.white,align:'center'});}
// Retain the note calls in source, but export empty speaker notes.
function note(s,_text=''){s.addNotes('');}
let s;
s=addBrandedTitleSlide(pptx,{
  courseLabel:'BEYOND MULTIPLE LINEAR REGRESSION · WEEK 2',
  mainTitle:'Cluster-randomized rehabilitation trials',
  author:'Dr Douwe Postmus',affiliation:'UMCG · Department of Epidemiology',badge:'BLR'
});
note(s,'The rehabilitation example uses simulated teaching data. This deck is the starting section for the revised Week 2 lecture.');
s=slide('Week 1: treatment randomized within centers','RECAP');
bullet(s,[
  'Each center included both rehabilitation programs.',
  'Treatment comparisons were made within centers.',
  'A random intercept represented the contribution shared by patients in a center.',
  'The model assumed the same treatment difference in every center.'
],.9,1.6,11.6,20,.9);
takeaway(s,'Now the whole center receives one rehabilitation program.',.9,5.65,11.6,.7);
s=addComparisonSlide(pptx,{
  title:'Patient-level and center-level randomization',
  leftTitle:'RANDOMIZED BLOCK DESIGN',
  leftBullets:[
    'Randomize patients within each center.',
    'Each center includes both rehabilitation programs.',
    'Compare the programs within centers.'
  ],
  rightTitle:'CLUSTER-RANDOMIZED DESIGN',
  rightBullets:[
    'Randomize whole centers to a rehabilitation program.',
    'Every patient in a center receives its assigned program.',
    'Compare the programs between centers.'
  ]
});
note(s,'The clinical question, rehabilitation programs and 12-week outcome remain the same. The unit of treatment assignment changes from the patient to the center. A center-wide policy may make delivery practical or reduce contamination between programs. Source: Campbell et al. CONSORT extension for cluster randomized trials, BMJ 2012;345:e5661, https://www.bmj.com/content/345/bmj.e5661.');

s=addFigureSlide(pptx,{
  title:'One rehabilitation program per center',
  assetPath:path.join('assets','cluster-centers.png'),
  caption:`${R.centers} centers randomized, ${R.cluster.centers_per_arm} per program. Each center contributes ${R.cluster.patients_per_center} patient outcomes.`
});
note(s,'This is an alternative simulated cluster-randomized trial, not a relabeling of observed patient outcomes. The generation script retains the same center contributions, patient deviations and common 24 m treatment effect as the blocked scenario, and computes outcomes under the new assignments. Each dot is a patient and each diamond is a center mean. The data are for teaching and are not evidence from a real trial.');

s=slide('A random-intercept model for center-level treatment','CLUSTER RANDOMIZATION');
equation(s,'Y_{ij}=\\beta_0+\\beta_1T_j+c_j+\\varepsilon_{ij}',.8,1.45,11.6,.8);
txt(s,'Tⱼ = 0 for a standard-care center; Tⱼ = 1 for an enhanced-care center.',.9,2.42,11.6,.35,{fontSize:18,align:'center',color:C.muted});
panelText(s,'CENTER CONTRIBUTION','Patients in center j share cⱼ, which shifts their expected walking distance.',.9,3.1,5.4);
panelText(s,'TREATMENT DIFFERENCE','β₁ compares the mean outcomes under the two programs across centers.',6.75,3.1,5.5);
takeaway(s,'The model assumes a common treatment difference.\nEvery patient in a center shares its treatment assignment.',.9,5.6,11.6,.8);
note(s,'Patient i is nested in center j. T_j has one value per center, whereas T_ij varied between patients in the blocked trial. The random center intercepts have mean zero and variance sigma_c squared, patient errors have mean zero and variance sigma squared, and center intercepts and patient errors are independent in this model. Center effects are independent across centers and treatment is assigned independently of them by randomization. The model still assumes a common treatment effect. We observe only one program per center, so the treatment comparison is between centers. The R formula remains nlme::lme(distance ~ treatment, random = ~ 1 | center, data = cluster_study). A separate fixed indicator for every center would absorb the center-level treatment assignment; unlike the blocked design, that model cannot identify a separate treatment coefficient. Model reference: https://stat.ethz.ch/R-manual/R-devel/library/nlme/html/lme.html.');

s=slide('Treatment uncertainty in the cluster-randomized trial','CLUSTER RANDOMIZATION');
txt(s,`${R.centers} randomized centers, ${R.cluster.centers_per_arm} per program; ${R.cluster.total} patient outcomes.`,.9,1.15,11.6,.4,{fontSize:19,color:C.muted});
table(s,[['Analysis','Contrast','SE','95% CI'],
  ['Treatment only',`${fmt(R.cluster.ordinary.estimate)} m`,`${fmt(R.cluster.ordinary.se,2)} m`,ci(R.cluster.ordinary)],
  ['Random intercept',`${fmt(R.cluster.random.estimate)} m`,`${fmt(R.cluster.random.se,2)} m`,ci(R.cluster.random)]
],.9,1.95,[3.5,2.3,2.2,3.7],.78);
txt(s,`Random-intercept model: t(${R.cluster.random.df}) = ${fmt(R.cluster.random.t,2)}; p = ${fmt(R.cluster.random.p,3)}.`,.9,4.65,11.6,.45,{fontSize:20,color:C.navy});
takeaway(s,'The same treatment estimate has much greater uncertainty once center clustering is included.',.9,5.65,11.6,.8);
note(s,'All estimates, standard errors and intervals come from the separate simulated cluster dataset. Ordinary regression uses 478 residual degrees of freedom, treating patients as independent. The nlme random-intercept treatment test uses 4 denominator degrees of freedom because treatment varies only between six centers, with three per arm. The equal point estimates follow the equal center sizes in this example. The observed difference is not the generating 24 m effect because patient noise and chance allocation of centers also affect the observed contrast. With six randomized centers, inference about a treatment effect remains limited and depends on the stated model assumptions. Source: Campbell et al., BMJ 2012;345:e5661, https://www.bmj.com/content/345/bmj.e5661.');

s=addComparisonSlide(pptx,{
  title:'Where does treatment information come from?',
  leftTitle:'RANDOMIZED WITHIN CENTERS',
  leftText:'Each center provides a comparison between the programs.\n\nThe shared center contribution cancels in that comparison.\n\nCenter adjustment improves precision in this balanced additive example.',
  rightTitle:'RANDOMIZED BETWEEN CENTERS',
  rightText:'Each center receives only one program.\n\nDifferences between centers contribute to uncertainty about treatment.\n\nMore patients refine center means, while the number of randomized centers limits treatment information.'
});
note(s,'The comparison assumes a common treatment effect and the stated independent center and patient components. In the blocked example, exact balance and an additive center effect make the within-center contrast remove the shared intercept. In the cluster design, that same component contributes to the sampling variation of the treatment contrast. Accounting for clustering corrects the uncertainty estimate; it does not itself remove information from the trial. The direction of the standard-error change is not a general property of adding random effects. Source: Campbell et al., BMJ 2012;345:e5661, https://www.bmj.com/content/345/bmj.e5661.');

s=slide('Repeated trials under both randomization schemes','SIMULATOR DISCUSSION');
bullet(s,[
  'Keep the common treatment effect and variance components fixed.',
  'Compare treatment-only and random-intercept analyses in each design.',
  'Under no effect, compare rejection rates and interval coverage.',
  'Under a specified effect, examine power alongside calibration.'
],.9,1.55,11.6,20,.85);
takeaway(s,'The level of randomization changes how clustering affects treatment uncertainty.',.9,5.4,11.6,.65);
txt(s,'Sampling Simulator: Clustered data',.9,6.25,11.6,.4,{fontSize:18,color:C.teal,bold:true});
note(s,'Use the two Sampling Simulator routes #/concepts/randomized-block-design and #/concepts/cluster-randomization. Each uses six hospitals and 20 patients per hospital, with different outcome units from the rehabilitation lecture. Match effect and variance settings across views to compare designs rather than treating the simulator as a reproduction of the lecture numbers. The simulator implements ordinary regression and Gaussian random-intercept REML; it has no fixed-center comparison panel. It draws new center effects and patients across repeated trials. Under the stated model, ignoring centers can be conservative for exactly balanced within-center assignment and anti-conservative for cluster assignment. Compare calibration before comparing power.');

s=slide('Key ideas','SUMMARY');
bullet(s,[
  'Treatment assignment is shared by every patient in a randomized center.',
  'The treatment comparison is made between centers.',
  'A random intercept accounts for the contribution shared within each center.',
  'The number of randomized centers limits treatment information.'
],.9,1.6,11.6,20,.9);
txt(s,'The model retains a common treatment effect across centers.',.9,6.0,11.6,.6,{fontSize:20,bold:true,color:C.teal});
// Footer and layout checks.
pptx._slides.forEach((sl,i)=>{if(i>0)warnIfSlideHasOverlaps(sl,pptx,{ignoreLines:true});warnIfSlideElementsOutOfBounds(sl,pptx)});
writePresentation(pptx,'Beyond_MLR_Week_2.pptx').catch(error=>{console.error(error);process.exitCode=1;});
