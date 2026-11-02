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
const {addOmittedCenterSlide,addErrorInterlude}=require('./error_interlude');
const {addFixedCenterSection}=require('./fixed_center');
const {addMeanModelSlide,addErrorRecap}=require('./error_recap');
const {addStudyOverviewSlide,addDiagnosticsInterpretationSlide}=require('../../slides-common/js/course_builders');
const {defineTemplateMasters,addBase,addSlideTitle,addTextBox,addAssetFrame,addNoteBox,addBrandedTitleSlide,addSectionSlide,addBulletVisualSlide,addComparisonSlide,addFigureSlide}=primitives;
const pptx=new pptxgen(); pptx.layout='LAYOUT_WIDE'; pptx.author='Beyond MLR';pptx.subject='Week 1: a multicenter trial, treatment-only analysis and shared center contributions';pptx.title='Beyond MLR | Week 1';pptx.lang='en-GB';pptx.theme={headFontFace:FONT_FAMILIES.sans,bodyFontFace:FONT_FAMILIES.sans,lang:'en-GB'};defineTemplateMasters(pptx);
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
function bullet(s,items,x,y,w,fs=20,dy=.61){items.forEach((t,i)=>{txt(s,'–',x,y+i*dy,.2,.31,{fontSize:fs,color:C.teal});txt(s,t,x+.31,y+i*dy,w-.31,dy-.12,{fontSize:fs,valign:'top',breakLine:false});});}
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
function note(s,_text=''){s.addNotes('');}
let s;
// 1
s=addBrandedTitleSlide(pptx,{courseLabel:'BEYOND MULTIPLE LINEAR REGRESSION',mainTitle:'Treatment comparisons and shared centers',author:'Dr Douwe Postmus',affiliation:'UMCG · Department of Epidemiology',badge:'BLR'});note(s,'The rehabilitation data in this lecture are simulated teaching data.');
// 2
s=slide('Today’s questions');panelText(s,'01  INITIAL ANALYSIS','How do we estimate and interpret the treatment difference?',.7,1.75,5.6);panelText(s,'02  MODEL AND STUDY','Does the treatment-only model adequately represent the study?',6.85,1.75,5.6);panelText(s,'03  SHARED CENTERS','What does an omitted center contribution mean for the errors?',.7,3.7,5.6);panelText(s,'04  MODEL COMPARISON','How does center adjustment change uncertainty?',6.85,3.7,5.6);
//3
s=addSectionSlide(pptx,{sectionLabel:'PART 1',sectionTitle:'A multicenter trial: initial analysis'});
// Start with the full trial and its treatment-only analysis. Center-specific
// patterns and center-adjusted analyses remain in the later section.
s=addStudyOverviewSlide(pptx,{
  title:'A multicenter rehabilitation trial',studyTitle:'DESIGN AND OUTCOME',
  bullets:[
    `Simulated trial: ${initial.n} patients after knee replacement at ${initial.centers} centers.`,
    `Within each center, ${R.patients_per_arm} patients are randomized to each program.`,
    'Standard care: weekly physiotherapy and home exercises.',
    'Enhanced care: standard care plus a weekly supervised session.',
    'Outcome: six-minute walk distance at 12 weeks.'
  ],
  assetPath:path.join('assets','rehabilitation-walk-assessment.png'),
  question:'How much do the mean walking distances differ between the programs?'
});
note(s,'');

s=addFigureSlide(pptx,{
  title:'Walking distances pooled across centers',assetPath:path.join('assets','pooled-outcomes.png'),
  caption:`${initial.patients_per_arm} patients per program across all ${initial.centers} centers. Each point represents one patient.`
});

// Introduce the proposed expected outcomes and zero-mean error together.
s=addMeanModelSlide(pptx,equation);
addErrorRecap(pptx,equation);

// Complete the distributional specification before showing model estimates.
s=addBulletVisualSlide(pptx,{
  title:'Completing the statistical model',
  bulletTitle:'FOUR ASSUMPTIONS ABOUT THE ERRORS',
  bullets:[
    'Zero mean: each patient’s expected error is zero.',
    'Equal variance: error variance is the same for every patient.',
    'Independence: errors are independent across patients.',
    'Normality: the errors have Normal distributions.'
  ],
  bulletFontSize:21,bulletMinFontSize:21,
  assetPath:path.join('assets','conditional-distributions.png'),
  note:'These assumptions describe the error distribution and the relationships between patients’ errors.',
  noteHeight:1.05,noteFontSize:18
});
note(s,'');

s=slide('Initial treatment-only results','TREATMENT COMPARISON');
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
note(s,'');

s=addDiagnosticsInterpretationSlide(pptx,{
  title:'Checking the treatment-only model',
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
  takeaway:'These plots summarize residuals pooled across centers. They can look reassuring even when the model does not adequately represent differences between centers.'
});
note(s,'');
// End Part 1 with the omitted-center contribution to the error.
addOmittedCenterSlide(pptx,equation);

s=addSectionSlide(pptx,{sectionLabel:'PART 2',sectionTitle:'Repeated sampling with the same centers'});
addFixedCenterSection(pptx,equation,R);

// Existing randomized-block material retained for subsequent refinement.
s=addSectionSlide(pptx,{sectionLabel:'PART 3',sectionTitle:'Randomized blocks: treatment within centers'});
s=slide('The same trial across six centers','RANDOMIZED BLOCKS');bullet(s,['Six centers recruit patients from their local populations.',`In every center, ${R.patients_per_arm} patients receive each treatment.`,'Each patient has one 12-week walking-distance outcome.','Treatment is randomized within each center.'],.85,1.65,11.7,21,.85);txt(s,'What does a patient share with others in the same center?',.85,5.55,11.7,.65,{fontSize:23,bold:true,color:C.navy});
//14
s=slide('Outcomes differ across centers','RANDOMIZED BLOCKS');fig(s,'centers.png',.62,1.47,9.0,4.95);panelText(s,'GROUPING STRUCTURE','Each patient belongs to one center.',9.9,1.9,2.7);panelText(s,'TREATMENT','Both treatments are represented within every center.',9.9,3.7,2.7);note(s,'Points are simulated patient outcomes. Diamonds show observed treatment means within each center. Center differences here are teaching assumptions, not empirical findings.');
//15
s=slide('Within-center treatment comparisons','RANDOMIZED BLOCKS');fig(s,'center-means.png',.62,1.47,9.0,4.95);panelText(s,'EACH CENTER','Compare enhanced with standard in the same center.',9.85,1.95,2.8);panelText(s,'COMMON EFFECT','The model assumes a common treatment difference.',9.85,3.9,2.8);note(s,'Each line connects the standard and enhanced treatment means within one center. Horizontal separation represents the observed treatment contrast. These observed contrasts differ because of sampling variation, even though the data-generating model has the same underlying treatment effect in every center.');
//16
s=slide('A model with a center contribution','RANDOMIZED BLOCKS');
equation(s,'Y_{ij}=\\beta_0+\\beta_1T_{ij}+c_j+\\varepsilon_{ij}',.8,1.62,10.6,.8);
txt(s,'Patient i belongs to center j.',.9,2.51,11.6,.27,{fontSize:17,align:'center',color:C.muted});
panelText(s,'CENTER DIFFERENCES','cⱼ shifts both treatment means up or down within center j.',.9,3.02,5.4);
panelText(s,'COMMON TREATMENT DIFFERENCE','β₁ is the enhanced-versus-standard mean difference in every center.',6.75,3.02,5.5);
takeaway(s,'Centers can have different mean walking distances under standard care.\nThe enhanced-versus-standard mean difference is the same in every center.',.9,5.5,11.6,.8);
note(s,'The model allows centers to have different mean walking distances under standard care and assumes the same enhanced-versus-standard difference in every center. The center contribution shifts both treatment means by the same amount. Patients also have individual deviations around their center-and-treatment mean. These are outcomes at 12 weeks, not measurements taken before treatment. If center is omitted, its contribution remains in the error.');
//17
s=slide('Three analyses of the same trial','MODEL COMPARISON');table(s,[['Model','Treatment','Center'],['Treatment only','Fixed effect','Omitted'],['Fixed-center model','Fixed effect','Fixed effect'],['Random-intercept model','Fixed effect','Random effect']],.8,1.75,[4.6,3.3,3.9],.7);txt(s,'Compare the treatment estimate, standard error and remaining variation.',.9,5.45,11.6,.65,{fontSize:21,color:C.navy});
//18
s=addComparisonSlide(pptx,{title:'Fixed and random center effects',leftTitle:'FIXED CENTER',leftText:'Each center has its own mean adjustment.\n\nThe model estimates these adjustments separately.\n\nThe treatment difference is the same in every center.',rightTitle:'RANDOM CENTER',rightText:'Each center has a shift shared by its patients.\n\nThese shifts vary around zero with an estimated variance.\n\nThe treatment difference is the same in every center.'});note(s,'Both models allow centers to differ in their mean walking distance under standard care and assume the same treatment difference in every center. The fixed-center model estimates separate center parameters. The random-intercept model describes center shifts using a distribution with mean zero and variance sigma_c squared. The patient residual variance describes remaining within-center variation after accounting for treatment. The fixed and random models imply different assumptions and inferential frameworks. Avoid portraying the fixed/random choice solely as whether the specific groups are interesting.');
// Compare expected errors and covariance after introducing both formulations.
addErrorInterlude(pptx,equation);
//19
s=slide('Treatment estimate and uncertainty','MODEL COMPARISON');table(s,[['Analysis','Contrast','SE','95% CI'],...[["Treatment only",R.pooled],["Center fixed",R.fixed],["Center random",R.random]].map(([name,v])=>[name,`${fmt(v.estimate)} m`,`${fmt(v.se,2)} m`,ci(v)])],.9,1.65,[3.5,2.3,2.2,3.7],.73);takeaway(s,'Exact within-center balance gives the same treatment estimate.',.9,5.3,11.6,.6);txt(s,'Simulated trial; enhanced minus standard.',.9,6.0,11.5,.38,{fontSize:15,color:C.muted});note(s,'Numerical results calculated from the included simulated dataset. Equality of fixed and mixed standard errors applies to this balanced additive case with an interior random-effect variance estimate; it is not a general rule.');
//19b
s=slide('Why does center adjustment change precision?','MODEL COMPARISON');panelText(s,'TREATMENT ONLY',`Residual SD: ${fmt(R.pooled.residual_sd)} m\nCenter differences remain among the residuals.`,.85,1.7,5.45);panelText(s,'CENTER ADJUSTED',`Residual SD: ${fmt(R.fixed.residual_sd)} m\nThe comparison uses differences within centers.`,6.8,1.7,5.45);takeaway(s,`The treatment contrast remains ${fmt(R.fixed.estimate)} m in this balanced trial.`,.85,5.4,11.6,.6);note(s,'These SDs are the estimates from the actual simulated treatment-only and fixed-center linear models. Lower residual variance and balanced design reduce the standard error in this example; this is not guaranteed for every dataset.');
//19c
s=addFigureSlide(pptx,{title:'Residual variation with and without center',assetPath:path.join('assets','residual-variation.png'),caption:`Same ${R.total} patients and same treatment contrast. The estimated residual SD accounts for model degrees of freedom.`});note(s,'Histograms use the ordinary linear-model residuals. In this balanced simulated dataset, accounting for centers reduces residual spread. The residual SD printed is sigma(model), using the correct residual degrees of freedom.');
//20
s=slide('Where does unexplained variation go?','MODEL COMPARISON');panelText(s,'TREATMENT ONLY','One residual term contains patient differences and center-related differences.',.85,1.8,5.5);panelText(s,'RANDOM INTERCEPT','Variation is separated into a center component and a patient component.',6.8,1.8,5.5);equation(s,'\\operatorname{Var}(Y_{ij}\\mid T_{ij})=\\sigma^2_c+\\sigma^2',1.1,4.85,10.4,.67);txt(s,'After accounting for treatment, variation comes from differences between centers and differences within centers.',.9,5.85,11.6,.7,{fontSize:20,align:'center'});note(s,'The random intercept and residual are independent in this model; the formula applies to the marginal conditional variance given treatment, not a residual conditional on the center effect. Estimates from different models need not sum exactly.');
//21
s=slide('Intraclass correlation','MODEL COMPARISON');equation(s,'\\mathrm{ICC}=\\frac{\\sigma_c^2}{\\sigma_c^2+\\sigma^2}',.95,1.6,6.6,1);txt(s,'Estimated center variance',7.7,1.62,4.4,.32,{fontSize:17,color:C.muted});txt(s,`${fmt(R.random.center_var,0)} m²`,7.7,2.0,4.0,.45,{fontSize:25,bold:true});txt(s,'Estimated patient variance',7.7,2.85,4.4,.32,{fontSize:17,color:C.muted});txt(s,`${fmt(R.random.patient_var,0)} m²`,7.7,3.23,4.0,.45,{fontSize:25,bold:true});txt(s,`After accounting for treatment, about ${fmt(100*R.random.icc,0)}% of the remaining variation is between centers.`,.95,3.15,6.3,1.1,{fontSize:20});txt(s,`ICC ≈ ${fmt(R.random.icc,2)} in this simulated trial`,.95,5.25,11,.55,{fontSize:22,bold:true,color:C.teal});txt(s,'Patients in the same center have correlated outcomes after accounting for treatment.',.95,6.0,11.4,.65,{fontSize:19});note(s,'Here the ICC is both the proportion of marginal unexplained variance attributable to the random center intercept and the model-implied correlation of two distinct patients in the same center, conditional on their treatments.');
//22
s=slide('One trial and repeated trials','SIMULATOR DISCUSSION');bullet(s,['Start with one randomized-block trial: compare model estimates and intervals.','Repeat under no treatment effect: examine rejection frequency and coverage.','Repeat with a specified treatment effect: examine power.'],.9,1.68,11.6,21,1.0);txt(s,'Sampling Simulator: Clustered data → Randomised blocks',.9,5.5,11.6,.62,{fontSize:19,bold:true,color:C.teal});note(s,'Use the separate sampling-simulator application, route #/concepts/randomized-block-design. Its current analyses are ordinary regression and random-intercept REML; a separate fixed-center panel is not currently implemented. Each repetition draws new hospital effects and patients, and treatment is exactly balanced within hospitals.');
//23
s=slide('What changes across repeated trials?','SIMULATOR DISCUSSION');panelText(s,'NO TREATMENT EFFECT','Compare the rejection rate with the 5% reference level.',.85,1.85,5.5);panelText(s,'SPECIFIED EFFECT','Compare power alongside Type I error and interval coverage.',6.8,1.85,5.5);takeaway(s,'A larger rejection rate is not automatically better.',.85,5.35,11.6,.6);note(s,'Balanced blocked design under the specified random-intercept simulation: ordinary analysis can be conservative, while the random-intercept method is calibrated under its assumptions. Do not generalize direction to cluster-randomized designs.');
//24
s=slide('Lab 2: treatment within centers','LAB');bullet(s,['Visualize treatment outcomes and mean differences by center.','Fit the treatment-only and center-adjusted models.','Compare the treatment estimate and its uncertainty.','Interpret center and patient variance in the random-intercept model.'],.85,1.7,11.7,20,.85);txt(s,'Explain the analysis and interpretation.',.85,5.65,11.5,.55,{fontSize:21,bold:true,color:C.teal});
// Summary and transition to the Week 2 starter.
s=slide('Key ideas','SUMMARY');
bullet(s,[
  'A treatment regression model describes outcomes in each treatment group.',
  'Patients in the same center share a center contribution.',
  'Randomization within centers makes within-center treatment comparisons possible.',
  'Accounting for center can change uncertainty even when the treatment estimate is unchanged.'
],.85,1.65,11.7,20,.9);
txt(s,'Next: treatment assigned to whole centers.',.85,6.0,11.4,.5,{fontSize:21,bold:true,color:C.teal});
// Footer and layout checks.
pptx._slides.forEach((sl,i)=>{if(i>0)warnIfSlideHasOverlaps(sl,pptx,{ignoreLines:true});warnIfSlideElementsOutOfBounds(sl,pptx)});
writePresentation(pptx,'Beyond_MLR_Week_1.pptx').catch(error=>{console.error(error);process.exitCode=1;});
