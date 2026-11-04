const pptxgen = require('pptxgenjs');
const fs=require('fs'), path=require('path'), cp=require('child_process'), crypto=require('crypto');
const {latexToSvgDataUri}=require('./pptxgenjs_helpers/latex');
const {svgToDataUri}=require('./pptxgenjs_helpers/svg');
const {imageSizingContain}=require('./pptxgenjs_helpers/image');
const {warnIfSlideHasOverlaps,warnIfSlideElementsOutOfBounds}=require('./pptxgenjs_helpers/layout');
const {autoFontSize}=require('./pptxgenjs_helpers/text');
const {COLORS, FONT_FAMILIES}=require('../../slides-common/js/blr_theme');
const primitives=require('../../slides-common/js/pptx_layout_primitives');
const {writePresentation}=require('../../slides-common/js/write_presentation');
const {addTreatmentCenterModelSlide,addOmittedCenterSlide}=require('./error_interlude');
const {addFixedCenterSection}=require('./fixed_center');
const {addSimulationApproach,addFixedSimulationOpening}=require('./simulation_demonstration');
const {addRandomCenterModel}=require('./random_center_model');
const {addLab2Debrief}=require('./lab2_debrief');
const {addCourseIntroduction}=require('./course_introduction');
const {addTreatmentOnlyModelSlide}=require('./error_recap');
const {addStudyOverviewSlide,addDiagnosticsInterpretationSlide}=require('../../slides-common/js/course_builders');
const {defineTemplateMasters,addBase,addSlideTitle,addTextBox,addAssetFrame,addNoteBox,addBrandedTitleSlide,addSectionSlide,addBulletVisualSlide,addComparisonSlide,addFigureSlide}=primitives;
const pptx=new pptxgen(); pptx.layout='LAYOUT_WIDE'; pptx.author='Beyond MLR';pptx.subject='Modeling center effects and comparing treatments';pptx.title='Randomized block designs | Beyond MLR | Week 1';pptx.lang='en-GB';pptx.theme={headFontFace:FONT_FAMILIES.sans,bodyFontFace:FONT_FAMILIES.sans,lang:'en-GB'};defineTemplateMasters(pptx);
const C={navy:COLORS.primary,teal:COLORS.primary,orange:COLORS.accent,ink:COLORS.text,muted:COLORS.muted,pale:COLORS.surfaceMuted,line:COLORS.cardLine,white:COLORS.card,gray:COLORS.background};
const W=13.333,H=7.5;
const R=JSON.parse(fs.readFileSync(path.join('assets','model_results.json'),'utf8'));
const initial=JSON.parse(fs.readFileSync(path.join('assets','initial-analysis-results.json'),'utf8'));
const fmt=(v,n=1)=>Number(v).toFixed(n), ci=v=>`${fmt(v.lower)} to ${fmt(v.upper)}`;
const pvalue=v=>v < .001 ? '< .001' : fmt(v,3);
function txt(s,t,x,y,w,h,o={}){const face=o.fontFace||FONT_FAMILIES.sans, size=o.fontSize||20; const measured=autoFontSize(t,face,{x,y,w,h,fontSize:size,minFontSize:Math.max(8,size-1),maxFontSize:size,mode:'shrink',margin:0});s.addText(t,{...measured,color:C.ink,breakLine:false,valign:'mid',...o});}
function line(s,x1,y1,x2,y2,color=C.line,width=1){s.addShape(pptx.ShapeType.line,{x:x1,y:y1,w:x2-x1,h:y2-y1,line:{color,width}})}
function header(s,title,section='WEEK 1'){addBase(s);addSlideTitle(s,title);}
function footer(s,n){txt(s,'BEYOND MLR · WEEK 1',10.15,7.18,2.1,.16,{fontSize:8,color:C.muted,align:'right'});txt(s,String(n),12.35,7.18,.35,.16,{fontSize:8,color:C.muted,align:'right'});}
function slide(title,section){let s=pptx.addSlide();header(s,title,section);return s;}
function bullet(s,items,x,y,w,fs=20,dy=.61){items.forEach((t,i)=>{txt(s,'•',x,y+i*dy,.2,.31,{fontSize:fs,color:C.teal});txt(s,t,x+.31,y+i*dy,w-.31,dy-.12,{fontSize:fs,valign:'top',breakLine:false});});}
function fig(s,file,x,y,w,h){addAssetFrame(s,{box:{x,y,w,h},assetPath:path.join('assets',file),fit:'contain'});}
function panelText(s,label,body,x,y,w){addTextBox(s,{box:{x:x-.13,y:y-.14,w:w+.2,h:1.78},title:label,text:body,fontSize:19,minFontSize:17});}
function equation(s,tex,x,y,w,h){
  const hash=crypto.createHash('sha1').update(tex).digest('hex').slice(0,12);
  const dir=path.join('assets','equations');fs.mkdirSync(dir,{recursive:true});
  const data=latexToSvgDataUri(tex); // LaTeX-to-SVG via the bundled helper.
  const svgPath=path.join(dir,'mathjax_'+hash+'.svg');
  if(!fs.existsSync(svgPath)) fs.writeFileSync(svgPath,Buffer.from(data.split(',')[1],'base64'));
  const pngPath=path.join(dir,'eq_'+hash+'.png');
  if(!fs.existsSync(pngPath)) throw new Error('Missing LaTeX PNG fallback: '+pngPath);
  s.addImage({path:pngPath,...imageSizingContain(pngPath,x,y,w,h)});
}

function table(s,rows,x,y,widths,rowH=.5){let yy=y;rows.forEach((row,r)=>{const sw=widths.reduce((a,b)=>a+b,0);/* Row background intentionally contains table text. */ s.addShape(pptx.ShapeType.rect,{x,y:yy,w:sw,h:rowH,line:{color:r?C.line:C.navy,width:.35},fill:{color:r?(r%2?C.white:'E6F0ED'):C.navy}});let xx=x;row.forEach((v,c)=>{txt(s,String(v),xx+.1,yy+.04,widths[c]-.2,rowH-.08,{fontSize:r?17:14,bold:r===0,color:r===0?C.white:C.ink,align:c?'center':'left'});xx+=widths[c];});yy+=rowH;});}
function takeaway(s,text,x,y,w,h=.48){/* Filled panel intentionally contains the takeaway text. */ s.addShape(pptx.ShapeType.roundRect,{x,y,w,h,rectRadius:.06,line:{color:C.navy,width:.3},fill:{color:C.navy}});txt(s,text,x+.18,y+.04,w-.36,h-.08,{fontSize:17,bold:true,color:C.white,align:'center'});}
// Retain the note calls in source, but export empty speaker notes.
function note(s){s.addNotes('');}
let s;
// 1
s=addBrandedTitleSlide(pptx,{courseLabel:'BEYOND MULTIPLE LINEAR REGRESSION',mainTitle:'Randomized block designs',subtitle:'Modeling center effects and comparing treatments',author:'Dr Douwe Postmus',affiliation:'UMCG · Department of Epidemiology',badge:'BLR'});note(s);
// Course-wide introduction precedes Part 1.
addCourseIntroduction(pptx);
//3
s=addSectionSlide(pptx,{sectionLabel:'PART 1',sectionTitle:'A multicenter trial: initial analysis'});
// Start with the full trial and its treatment-only analysis. Center-specific
// patterns and center-adjusted analyses remain in the later section.
s=addStudyOverviewSlide(pptx,{
  title:'A multicenter rehabilitation trial',studyTitle:'DESIGN AND OUTCOME',
  bullets:[
    `Simulated trial: ${initial.n} patients after knee replacement at ${initial.centers} centers.`,
    `Randomized block design: centers are the blocks. Within each center, ${R.patients_per_arm} patients are randomized to each treatment.`,
    'Standard care: weekly physiotherapy and home exercises.',
    'Enhanced care: standard care plus a weekly supervised session.',
    'Outcome: six-minute walk distance at 12 weeks.'
  ],
  assetPath:path.join('assets','rehabilitation-walk-assessment.png'),
  question:'How much do the mean walking distances differ between the programs?'
});
note(s);

s=addFigureSlide(pptx,{
  title:'Walking distances pooled across centers',assetPath:path.join('assets','pooled-outcomes.png'),
  caption:`${initial.patients_per_arm} patients per program across all ${initial.centers} centers.`,
  captionFontSize:18,captionMinFontSize:18
});

// Specify the complete model once; OLS review is covered in the homework.
s=addTreatmentOnlyModelSlide(pptx,equation);
note(s);

s=slide('Model results','TREATMENT COMPARISON');
table(s,[['Term','Estimate (m)','SE (m)','95% CI (m)','p-value'],
  ['Intercept (β₀)',fmt(initial.intercept.estimate),fmt(initial.intercept.se,2),ci(initial.intercept),pvalue(initial.intercept.p)],
  ['Enhanced care (β₁)',fmt(initial.treatment.estimate),fmt(initial.treatment.se,2),ci(initial.treatment),pvalue(initial.treatment.p)]
],.9,1.50,[3.9,1.9,1.6,2.8,1.4],.73);
txt(s,`Pooling across centers, patients assigned to enhanced care walked ${fmt(initial.treatment.estimate)} m farther on average.`,.9,4.10,11.6,.85,{fontSize:22});
addNoteBox(s,{
  box:{x:.9,y:5.35,w:11.6,h:1.0},
  text:`Estimated residual variance: ${fmt(initial.residual_var)} m².\nCorresponding residual SD: ${fmt(initial.residual_sd)} m.`,
  fontSize:20,minFontSize:19
});
note(s);

s=addDiagnosticsInterpretationSlide(pptx,{
  title:'Model diagnostics',
  diagnostics:[
    {
      assetPath:path.join('assets','pooled-residuals-vs-fitted.png'),
      purpose:'Assess whether residuals are centered around zero and have similar spread across fitted values.'
    },
    {
      assetPath:path.join('assets','pooled-normal-qq.png'),
      purpose:'Assess whether the residual distribution is approximately Normal.'
    }
  ],
  takeaway:'The residual plots do not suggest clear violations of the Normality or equal-variance assumptions.'
});
note(s);
// Part 2: specify the generating model, investigate treatment-only inference,
// explain the finding, then let the lab introduce the center-adjusted analysis.
s=addSectionSlide(pptx,{sectionLabel:'PART 2',sectionTitle:'Modeling center as a fixed effect'});
s=addBulletVisualSlide(pptx,{
  title:'A multicenter trial, a pooled analysis',
  textFraction:0.6,
  bullets:[
    'The trial used a randomized block design: patients were randomized to the two programs within each of six centers, with equal numbers per program in every center.',
    'The treatment-only model represents each treatment group as one homogeneous sample, without distinguishing the centers from which patients were recruited.',
    'Centers may differ in patient populations and care settings, shifting expected outcomes for both treatments.'
  ],
  bulletFontSize:21,bulletMinFontSize:21,
  assetPath:path.join('assets','rehabilitation-center-context.png'),
  framed:false,
  note:'Do center differences in expected outcomes matter for our treatment comparison?',
  noteHeight:1.05,noteFontSize:22
});
addSimulationApproach(pptx);
addTreatmentCenterModelSlide(pptx,equation);
addFixedSimulationOpening(pptx);
addOmittedCenterSlide(pptx,equation);
addFixedCenterSection(pptx,equation,R);

// Move from the included centers to modeling variation in a population of centers.
s=addSectionSlide(pptx,{sectionLabel:'PART 3',sectionTitle:'Modeling center as a random effect'});
addRandomCenterModel(pptx,equation);
addLab2Debrief(pptx,equation,R);
// End with the Lab 2 debrief before opening the discussion.
s=addSectionSlide(pptx,{sectionLabel:'The End',sectionTitle:'Questions'});
// Footer and layout checks.
pptx._slides.forEach((sl,i)=>{sl.addNotes('');if(i>0)warnIfSlideHasOverlaps(sl,pptx,{ignoreLines:true});warnIfSlideElementsOutOfBounds(sl,pptx)});
writePresentation(pptx,'Beyond_MLR_Week_1.pptx').catch(error=>{console.error(error);process.exitCode=1;});
