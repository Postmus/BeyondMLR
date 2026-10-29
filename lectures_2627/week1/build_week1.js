const pptxgen = require('pptxgenjs');
const fs=require('fs'), path=require('path'), cp=require('child_process'), crypto=require('crypto');
const {latexToSvgDataUri}=require('./pptxgenjs_helpers/latex');
const {svgToDataUri}=require('./pptxgenjs_helpers/svg');
const {imageSizingContain}=require('./pptxgenjs_helpers/image');
const {warnIfSlideHasOverlaps,warnIfSlideElementsOutOfBounds}=require('./pptxgenjs_helpers/layout');
const {autoFontSize}=require('./pptxgenjs_helpers/text');
const {COLORS, FONT_FAMILIES}=require('../../slides-common/js/blr_theme');
const primitives=require('../../slides-common/js/pptx_layout_primitives');
const {addStudyOverviewSlide,addConditionalModelSlide}=require('../../slides-common/js/course_builders');
const {defineTemplateMasters,addBase,addSlideTitle,addTextBox,addAssetFrame,addNoteBox,addBrandedTitleSlide,addSectionSlide,addBulletVisualSlide,addComparisonSlide,addFigureSlide}=primitives;
const pptx=new pptxgen(); pptx.layout='LAYOUT_WIDE'; pptx.author='Beyond MLR';pptx.subject='Week 1: independent-samples t-test and randomized blocks';pptx.title='Beyond MLR | Week 1';pptx.lang='en-GB';pptx.theme={headFontFace:FONT_FAMILIES.sans,bodyFontFace:FONT_FAMILIES.sans,lang:'en-GB'};defineTemplateMasters(pptx);
const C={navy:COLORS.primary,teal:COLORS.primary,orange:COLORS.accent,ink:COLORS.text,muted:COLORS.muted,pale:COLORS.surfaceMuted,line:COLORS.cardLine,white:COLORS.card,gray:COLORS.background};
const W=13.333,H=7.5;
const R=JSON.parse(fs.readFileSync(path.join('assets','model_results.json'),'utf8'));
const fmt=(v,n=1)=>Number(v).toFixed(n), ci=v=>`${fmt(v.lower)} to ${fmt(v.upper)}`;
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
  fs.writeFileSync(svgPath,Buffer.from(data.split(',')[1],'base64'));
  const pngPath=path.join(dir,'eq_'+hash+'.png');
  if(!fs.existsSync(pngPath)) throw new Error('Missing LaTeX PNG fallback: '+pngPath);
  s.addImage({path:pngPath,...imageSizingContain(pngPath,x,y,w,h)});
}

function table(s,rows,x,y,widths,rowH=.5){let yy=y;rows.forEach((row,r)=>{const sw=widths.reduce((a,b)=>a+b,0);/* Row background intentionally contains table text. */ s.addShape(pptx.ShapeType.rect,{x,y:yy,w:sw,h:rowH,line:{color:r?C.line:C.navy,width:.35},fill:{color:r?(r%2?C.white:'E6F0ED'):C.navy}});let xx=x;row.forEach((v,c)=>{txt(s,String(v),xx+.1,yy+.04,widths[c]-.2,rowH-.08,{fontSize:r?17:14,bold:r===0,color:r===0?C.white:C.ink,align:c?'center':'left'});xx+=widths[c];});yy+=rowH;});}
function takeaway(s,text,x,y,w,h=.48){/* Filled panel intentionally contains the takeaway text. */ s.addShape(pptx.ShapeType.roundRect,{x,y,w,h,rectRadius:.06,line:{color:C.navy,width:.3},fill:{color:C.navy}});txt(s,text,x+.18,y+.04,w-.36,h-.08,{fontSize:17,bold:true,color:C.white,align:'center'});}
function note(s,text){s.addNotes(text);}
let s;
// 1
s=addBrandedTitleSlide(pptx,{courseLabel:'BEYOND MULTIPLE LINEAR REGRESSION',mainTitle:'Treatment comparisons and shared centers',author:'Dr Douwe Postmus',affiliation:'UMCG · Department of Epidemiology',badge:'BLR'});note(s,'The rehabilitation data in this lecture are simulated teaching data.');
// 2
s=slide('Today’s questions');panelText(s,'01  TREATMENT GROUPS','How do the outcome distributions and treatment means differ?',.7,1.75,5.6);panelText(s,'02  SHARED CENTERS','What changes when patients receive care in several centers?',6.85,1.75,5.6);panelText(s,'03  MODEL COMPARISON','What do center adjustment and repeated trials tell us about uncertainty?',.7,3.7,5.6);panelText(s,'04  R AND QUARTO','How can we report an analysis reproducibly?',6.85,3.7,5.6);
//3
s=addSectionSlide(pptx,{sectionLabel:'PART 1',sectionTitle:'A single-center randomized trial'});
s=addStudyOverviewSlide(pptx,{title:'A randomized rehabilitation study',studyTitle:'STUDY DESIGN AND PRIMARY OUTCOME',bullets:[`Single-center trial: ${2*R.patients_per_arm} patients after knee replacement are randomized.`, 'Standard: weekly physiotherapy and home exercises for six weeks.', 'Enhanced: standard care plus a weekly supervised session focusing on walking, strength and balance.', 'Primary outcome: six-minute walk distance at 12 weeks.'],assetPath:path.join('assets','rehabilitation-walk-assessment.png'),question:'How much do the mean walking distances differ between the two programs?'});note(s,'The patient data and clinical case were simulated for teaching. The image is an AI-generated illustration, not a photograph from an actual trial.');
//4
s=addBulletVisualSlide(pptx,{title:'Exploratory data analysis',bulletTitle:'OBSERVED OUTCOME',bullets:[`${2*R.patients_per_arm} patients; ${R.patients_per_arm} per treatment.`,'Compare the centers of the distributions.','Consider the spread and unusual observations.'],assetPath:path.join('assets','single-center.png'),note:'Each point represents one patient. The outcome is 6-minute walk distance.'});
// The fitted distributions and their model appear together.
s=addConditionalModelSlide(pptx,{title:'Outcome distribution conditional on treatment',assetPath:path.join('assets','conditional-distributions.png'),equation:'Y_i\\mid T_i=t\\sim\\mathcal{N}(\\mu_t,\\sigma^2)',drawEquation:equation,bullets:['Each treatment has its own mean walking distance.','The model assumes a common within-treatment variance.','Individual outcomes vary around their treatment mean.'],note:'Conceptual model distributions; dashed lines indicate the treatment means.'});
//6
s=slide('Independent-samples t-test as linear regression','TREATMENT COMPARISON');
equation(s,'Y_i=\\beta_0+\\beta_1T_i+\\varepsilon_i,\\quad \\varepsilon_i\\sim N(0,\\sigma^2)',.8,1.03,11.7,.52);
txt(s,'Patient i:  Tᵢ = 0 for standard treatment;  Tᵢ = 1 for enhanced treatment.',.9,1.68,11.5,.31,{fontSize:17,align:'center',color:C.muted});
// The shift-rule panel intentionally contains its label, explanation, and equation.
s.addShape(pptx.ShapeType.roundRect,{x:.75,y:2.12,w:11.82,h:1.13,rectRadius:.06,line:{color:C.line,width:.7},fill:{color:C.white}});
txt(s,'NORMAL SHIFT RULE',1.0,2.31,4.7,.24,{fontSize:14,bold:true,color:C.navy});
txt(s,'Adding a constant shifts the mean, but leaves the variance unchanged.',1.0,2.69,6.2,.31,{fontSize:17});
equation(s,'X\\sim N(\\mu,\\sigma^2)\\quad\\Longrightarrow\\quad a+X\\sim N(a+\\mu,\\sigma^2)',7.25,2.42,4.95,.55);
txt(s,'For a given treatment, β₀ + β₁Tᵢ is the constant added to the error.',.9,3.43,11.5,.3,{fontSize:17,align:'center'});
equation(s,'Y_i\\sim N(\\beta_0+\\beta_1T_i,\\sigma^2)',2.0,3.90,9.3,.56);
// Group panels intentionally contain headings, distribution equations, and mean mappings.
s.addShape(pptx.ShapeType.roundRect,{x:.75,y:4.78,w:5.74,h:1.68,rectRadius:.06,line:{color:C.line,width:.7},fill:{color:C.white}});
s.addShape(pptx.ShapeType.roundRect,{x:6.84,y:4.78,w:5.74,h:1.68,rectRadius:.06,line:{color:C.line,width:.7},fill:{color:C.white}});
txt(s,'STANDARD TREATMENT',1.0,4.98,5.24,.27,{fontSize:15,bold:true,color:C.navy,align:'center'});
txt(s,'ENHANCED TREATMENT',7.09,4.98,5.24,.27,{fontSize:15,bold:true,color:C.navy,align:'center'});
equation(s,'Y_i\\mid T_i=0\\sim N(\\beta_0,\\sigma^2)',1.02,5.45,5.18,.42);
equation(s,'Y_i\\mid T_i=1\\sim N(\\beta_0+\\beta_1,\\sigma^2)',7.11,5.45,5.18,.42);
equation(s,'\\mu_0=\\beta_0',1.4,6.02,4.4,.29);
equation(s,'\\mu_1=\\beta_0+\\beta_1',7.49,6.02,4.4,.29);
note(s,'The model describes the outcome distribution for a given treatment. For patient i, the term β₀ + β₁Tᵢ shifts the Normal error distribution. The two columns match the treatment-specific means and distributions on the previous slide. Independence across patients is a separate assumption.');
//8
s=slide('Estimated means and treatment contrast','TREATMENT COMPARISON');table(s,[['Quantity','Estimate'],['Standard mean',`${fmt(R.single.standard_mean)} m`],['Enhanced mean',`${fmt(R.single.enhanced_mean)} m`],['Enhanced − standard',`${fmt(R.single.estimate)} m`],['95% confidence interval',`${ci(R.single)} m`]],1.0,1.6,[5.9,3.0],.65);txt(s,'The interval expresses uncertainty about the mean difference.',1.0,5.45,11,.5,{fontSize:19,color:C.muted});note(s,`Simulated center A, ${R.patients_per_arm} patients per group. Contrast and interval from the ordinary model with ${R.single.df} df; rounding accounts for difference in displayed means.`);
//9
s=slide('Estimated marginal means in R','TREATMENT COMPARISON');txt(s,'model <- lm(distance ~ treatment, data = study)',.95,1.78,11,.48,{fontFace:'Liberation Mono',fontSize:19,color:C.navy});txt(s,'means <- emmeans(model, ~ treatment)',.95,2.55,11,.48,{fontFace:'Liberation Mono',fontSize:19,color:C.navy});txt(s,'contrast(means, list("Enhanced - standard" = c(-1, 1)))',.95,3.32,11.5,.48,{fontFace:'Liberation Mono',fontSize:17,color:C.navy});line(s,.95,4.35,12.25,4.35);takeaway(s,'Means and contrasts answer the study question directly.',.95,4.68,11.3,.62);note(s,'Assumes emmeans is loaded, treatment levels are Standard then Enhanced, and study contains the single-center data. The explicitly specified contrast reports enhanced minus standard.');
//10
s=slide('Testing the treatment difference','TREATMENT COMPARISON');panelText(s,'INDEPENDENT-SAMPLES T-TEST',`H₀: equal treatment means.\nt(${R.single.df}) = ${fmt(R.single.t,2)}; p = ${fmt(R.single.p,3)}.`,.85,1.75,5.5);panelText(s,'ESTIMATED CONTRAST',`Enhanced − standard: ${fmt(R.single.estimate)} m.\n95% CI: ${ci(R.single)} m.`,6.85,1.75,5.5);takeaway(s,'The regression coefficient gives the equal-variance independent-samples t-test.',.85,5.5,11.6,.65);note(s,'The two-sided t-test, estimated contrast, and confidence interval are calculated from the same simulated center A outcomes. The model assumes a common within-treatment variance; the t-test of the treatment coefficient is the equal-variance independent-samples t-test.');
//11a
s=slide('Model assumptions: assessment in this trial','TREATMENT COMPARISON');table(s,[['Assumption','Evidence or design consideration'],['Conditional mean','The model estimates a separate mean for each treatment.'],['Constant variance',`Observed SD: ${fmt(R.single.standard_sd)} m (standard), ${fmt(R.single.enhanced_sd)} m (enhanced).`],['Normal errors','Use the Q–Q plot to assess departures from Normality.'],['Independent errors','Consider recruitment and measurement; randomization alone does not establish independence.']],.72,1.2,[3.4,8.45],.84);takeaway(s,'Plots and design information inform an assessment; they cannot prove the assumptions.',.85,6.0,11.6,.62);note(s,'Sample SDs are computed from the simulated center A data. With a single binary treatment predictor the model has a separate population mean per group, so there is no further functional shape to assess within this mean model. A Q-Q plot on pooled residuals is descriptive and a small sample limits what it can reveal.');
//11
s=slide('Model diagnostics','TREATMENT COMPARISON');fig(s,'diagnostics.png',.7,1.25,11.9,4.4);takeaway(s,'Residuals inform model checks; independence also requires the study design.',.85,5.88,11.6,.65);note(s,'For a single categorical predictor, fitted values are just group means; a residual-versus-fitted plot cannot diagnose a curved covariate effect. Independence is not established by a Q-Q or residual-versus-fitted plot.');
//12
s=slide('Lab 1: treatment comparisons','LAB');bullet(s,['Explore the outcome in each treatment group.','Fit a treatment regression model and interpret estimated means and contrasts.','Inspect model diagnostics.','Compare coding schemes in the lab; report a Quarto analysis.'],.9,1.8,11.4,21,.86);txt(s,'The same fitted model can be expressed using different coding schemes.',.9,5.65,11.4,.5,{fontSize:19,color:C.teal});
//13
s=addSectionSlide(pptx,{sectionLabel:'PART 2',sectionTitle:'Treatment within centers'});
s=slide('The same trial across six centers','RANDOMIZED BLOCKS');bullet(s,['Six centers recruit patients from their local populations.',`In every center, ${R.patients_per_arm} patients receive each treatment.`,'Each patient has one 12-week walking-distance outcome.','Treatment is randomized within each center.'],.85,1.65,11.7,21,.85);txt(s,'What does a patient share with others in the same center?',.85,5.55,11.7,.65,{fontSize:23,bold:true,color:C.navy});
//14
s=slide('Outcomes differ across centers','RANDOMIZED BLOCKS');fig(s,'centers.png',.62,1.47,9.0,4.95);panelText(s,'GROUPING STRUCTURE','Each patient belongs to one center.',9.9,1.9,2.7);panelText(s,'TREATMENT','Both treatments are represented within every center.',9.9,3.7,2.7);note(s,'Points are simulated patient outcomes. Diamonds show observed treatment means within each center. Center differences here are teaching assumptions, not empirical findings.');
//15
s=slide('Within-center treatment comparisons','RANDOMIZED BLOCKS');fig(s,'center-means.png',.62,1.47,9.0,4.95);panelText(s,'EACH CENTER','Compare enhanced with standard in the same center.',9.85,1.95,2.8);panelText(s,'COMMON EFFECT','The additive model summarizes these comparisons.',9.85,3.9,2.8);note(s,'Each line connects the standard and enhanced treatment means within one center. Horizontal separation represents the observed treatment contrast.');
//16
s=slide('A model with a center contribution','RANDOMIZED BLOCKS');equation(s,'Y_{ij}=\\beta_0+\\beta_1T_{ij}+c_j+\\varepsilon_{ij}',.8,1.62,10.6,.8);panelText(s,'i  PATIENT','The outcome belongs to a patient within a center.',.9,3.02,5.4);panelText(s,'j  CENTER','cⱼ represents the contribution shared within center j.',6.75,3.02,5.5);takeaway(s,'If center is omitted, its contribution remains in the error.',.9,5.5,11.6,.6);
//17
s=slide('Three analyses of the same trial','MODEL COMPARISON');table(s,[['Model','Treatment','Center'],['Treatment only','Fixed effect','Omitted'],['Fixed-center model','Fixed effect','Fixed effect'],['Random-intercept model','Fixed effect','Random effect']],.8,1.75,[4.6,3.3,3.9],.7);txt(s,'Compare the treatment estimate, standard error and remaining variation.',.9,5.45,11.6,.65,{fontSize:21,color:C.navy});
//18a
s=slide('Fitting the three models in R','MODEL COMPARISON');txt(s,'lm(distance ~ treatment, data = study)',.85,1.75,11.7,.48,{fontFace:'Liberation Mono',fontSize:19,color:C.navy});txt(s,'lm(distance ~ treatment + center, data = study)',.85,2.65,11.7,.48,{fontFace:'Liberation Mono',fontSize:18,color:C.navy});txt(s,'lme(distance ~ treatment, random = ~ 1 | center,',.85,3.55,11.7,.48,{fontFace:'Liberation Mono',fontSize:18,color:C.navy});txt(s,'    data = study)',.85,4.03,11.7,.48,{fontFace:'Liberation Mono',fontSize:18,color:C.navy});txt(s,'The random-intercept example uses the nlme package.',.85,5.55,11.6,.45,{fontSize:18,color:C.muted});note(s,'The included data-generation script fits these same models. A comparable formulation with lmer is distance ~ treatment + (1 | center).');
//18
s=addComparisonSlide(pptx,{title:'Fixed and random center effects',leftTitle:'FIXED CENTER',leftText:'A separate mean adjustment for each participating center.\n\nThe treatment contrast is estimated within centers.',rightTitle:'RANDOM CENTER',rightText:'Center contributions follow a distribution with an estimated variance.\n\nThe patient residual variance describes remaining within-center variation.'});note(s,'The fixed and random models imply different assumptions and inferential frameworks. Avoid portraying the fixed/random choice solely as whether the specific groups are interesting.');
//19
s=slide('Treatment estimate and uncertainty','MODEL COMPARISON');table(s,[['Analysis','Contrast','SE','95% CI'],...[["Treatment only",R.pooled],["Center fixed",R.fixed],["Center random",R.random]].map(([name,v])=>[name,`${fmt(v.estimate)} m`,`${fmt(v.se,2)} m`,ci(v)])],.9,1.65,[3.5,2.3,2.2,3.7],.73);takeaway(s,'Exact within-center balance gives the same treatment estimate.',.9,5.3,11.6,.6);txt(s,'Simulated trial; enhanced minus standard.',.9,6.0,11.5,.38,{fontSize:15,color:C.muted});note(s,'Numerical results calculated from the included simulated dataset with R lm and nlme::lme REML. Equality of fixed and mixed standard errors applies to this balanced additive case with an interior random-effect variance estimate; it is not a general rule.');
//19b
s=slide('Why does center adjustment change precision?','MODEL COMPARISON');panelText(s,'TREATMENT ONLY',`Residual SD: ${fmt(R.pooled.residual_sd)} m\nCenter differences remain among the residuals.`,.85,1.7,5.45);panelText(s,'CENTER ADJUSTED',`Residual SD: ${fmt(R.fixed.residual_sd)} m\nThe comparison uses differences within centers.`,6.8,1.7,5.45);takeaway(s,`The treatment contrast remains ${fmt(R.fixed.estimate)} m in this balanced trial.`,.85,5.4,11.6,.6);note(s,'These SDs are the estimates from the actual simulated treatment-only and fixed-center linear models. Lower residual variance and balanced design reduce the standard error in this example; this is not guaranteed for every dataset.');
//19c
s=addFigureSlide(pptx,{title:'Residual variation with and without center',assetPath:path.join('assets','residual-variation.png'),caption:`Same ${R.total} patients and same treatment contrast. The estimated residual SD accounts for model degrees of freedom.`});note(s,'Histograms use the ordinary linear-model residuals. In this balanced simulated dataset, accounting for centers reduces residual spread. The residual SD printed is sigma(model), using the correct residual degrees of freedom.');
//20
s=slide('Where does unexplained variation go?','MODEL COMPARISON');panelText(s,'TREATMENT ONLY','One residual term contains patient differences and center-related differences.',.85,1.8,5.5);panelText(s,'RANDOM INTERCEPT','Variation is separated into a center component and a patient component.',6.8,1.8,5.5);equation(s,'\\operatorname{Var}(Y_{ij}\\mid T_{ij})=\\sigma^2_c+\\sigma^2',1.1,4.85,10.4,.67);note(s,'The random intercept and residual are independent in this model; the formula applies to the marginal conditional variance given treatment, not a residual conditional on the center effect. Estimates from different models need not sum exactly.');
//21
s=slide('Intraclass correlation','MODEL COMPARISON');equation(s,'\\mathrm{ICC}=\\frac{\\sigma_c^2}{\\sigma_c^2+\\sigma^2}',.95,1.6,6.6,1);txt(s,'Estimated center variance',7.7,1.62,4.4,.32,{fontSize:17,color:C.muted});txt(s,`${fmt(R.random.center_var,0)} m²`,7.7,2.0,4.0,.45,{fontSize:25,bold:true});txt(s,'Estimated patient variance',7.7,2.85,4.4,.32,{fontSize:17,color:C.muted});txt(s,`${fmt(R.random.patient_var,0)} m²`,7.7,3.23,4.0,.45,{fontSize:25,bold:true});txt(s,`ICC ≈ ${fmt(R.random.icc,2)} in this simulated trial`,.95,5.25,11,.55,{fontSize:22,bold:true,color:C.teal});note(s,'Here the ICC is both the proportion of marginal unexplained variance attributable to the random center intercept and the model-implied correlation of two distinct patients in the same center, conditional on their treatments.');
//22
s=slide('One trial and repeated trials','SIMULATOR DISCUSSION');bullet(s,['Start with one randomized-block trial: compare model estimates and intervals.','Repeat under no treatment effect: examine rejection frequency and coverage.','Repeat with a specified treatment effect: examine power.'],.9,1.68,11.6,21,1.0);txt(s,'Sampling Simulator: Clustered data → Randomised blocks',.9,5.5,11.6,.62,{fontSize:19,bold:true,color:C.teal});note(s,'Use the separate sampling-simulator application, route #/concepts/randomized-block-design. Its current analyses are ordinary regression and random-intercept REML; a separate fixed-center panel is not currently implemented. Each repetition draws new hospital effects and patients, and treatment is exactly balanced within hospitals.');
//23
s=slide('What changes across repeated trials?','SIMULATOR DISCUSSION');panelText(s,'NO TREATMENT EFFECT','Compare the rejection rate with the 5% reference level.',.85,1.85,5.5);panelText(s,'SPECIFIED EFFECT','Compare power alongside Type I error and interval coverage.',6.8,1.85,5.5);takeaway(s,'A larger rejection rate is not automatically better.',.85,5.35,11.6,.6);note(s,'Balanced blocked design under the specified random-intercept simulation: ordinary analysis can be conservative, while the random-intercept method is calibrated under its assumptions. Do not generalize direction to cluster-randomized designs.');
//24
s=slide('Lab 2: treatment within centers','LAB');bullet(s,['Visualize treatment outcomes and mean differences by center.','Fit the treatment-only and center-adjusted models.','Compare the treatment estimate and its uncertainty.','Interpret center and patient variance in the random-intercept model.'],.85,1.7,11.7,20,.85);txt(s,'Report the analysis and interpretation in Quarto.',.85,5.65,11.5,.55,{fontSize:21,bold:true,color:C.teal});
//25
s=slide('Key ideas','SUMMARY');bullet(s,['A treatment regression model describes the outcome distribution in each group.','Estimated means and contrasts keep the scientific comparison visible.','Within-center randomization makes treatment comparisons possible within centers.','Accounting for center can change uncertainty even when the treatment estimate is unchanged.'],.85,1.65,11.7,20,.9);txt(s,'Next: can treatment effects differ across centers?',.85,6.0,11.4,.5,{fontSize:21,bold:true,color:C.teal});
// footer and required layout checks
pptx._slides.forEach((sl,i)=>{if(i>0)warnIfSlideHasOverlaps(sl,pptx,{ignoreLines:true});warnIfSlideElementsOutOfBounds(sl,pptx)});
pptx.writeFile({fileName:'Beyond_MLR_Week_1.pptx'});
