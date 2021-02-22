function dispHelp(index)
% This function displays a help screen and sets up controls that allow the
% user to navigate from one help screen to another. There are 5 possible screens.
% depending on the index value, the correct title is displayed and the correct
% block of text is displayed. If the window is currently open, this function just
% updates the title and text block.


global hFIG			% array of handles to figures
global TRUE
global FALSE

TitleString(1,:)='      OverView of BAZ       ';
TitleString(2,:)='     Main Screen of BAZ     ';
TitleString(3,:)='Filtering Seismograms in BAZ';
TitleString(4,:)='   Particle Motion Screen   ';
TitleString(5,:)='      Polarization Plot     ';

h7=findobj(0,'Tag','HelpWindow');
if isempty(h7)
   makeFigure(7,[300 200 500 400],TitleString(index,:));
   h7=hFIG(7);
   set(h7,'Resize','off','Tag','HelpWindow')
   set(gca,'position',[.05 .05 .62 .9])
   set(gca,'ydir','reverse')
   set(gca,'xlim',[0 1],'ylim',[0 3])
   set(gca,'yticklabel',[])
   set(gca,'xticklabel',[])
   set(gca,'xtick',[])
   set(gca,'ytick',[])
else
   set(h7,'Name',TitleString(index,:))
   h=findobj(h7,'Tag','hlpStr');
   for j=1:length(h)
      delete(h(j))
   end
   h=[];
end
   
if isempty( findobj(h7,'Tag','HelpFrame') )
   uicontrol('style','frame','position',[360 10 130 380],'Tag','HelpFrame','BackgroundColor',[.4 .4 .6])
   uicontrol('style','pushbutton','position',[370 350 110 30],'string','Overview','callback','dispHelp(1)')
   uicontrol('style','pushbutton','position',[370 310 110 30],'string','Main Screen','callback','dispHelp(2)')
   uicontrol('style','pushbutton','position',[370 270 110 30],'string','Filtering','callback','dispHelp(3)')
   uicontrol('style','pushbutton','position',[370 230 110 30],'string','Particle Motion','callback','dispHelp(4)')
   uicontrol('style','pushbutton','position',[370 190 110 30],'string','Polarization','callback','dispHelp(5)')
   uicontrol('style','pushbutton','position',[370 40 110 30],'string','Close','callback','hclose(7)')
end

if index == 1
    hlpStr= ...                                               
        ['                                            '
         ' The BAZ module provides a convenient       ' 
         ' environment for making back azimuth        '
         ' estimates from 3-component SAC seismograms.'
         '                                            '  
         ' BAZ calculates the back azimuth and the    '
         ' incidence angle of the signal within an    '
         ' adjustable window using a 3-axis principal '
         ' component method (Flinn, 1965). BAZ also   '
         ' allows interactive inspection of signal    '
         ' polarization using 3-D particle motion     '
         ' plots, interactive picking of regional     '
         ' phases, bandpass filtering of the signals, '
         ' rotation of channel components, and display'
         ' of polarization versus time computed as    '
         ' ML probability (Christofferson et al.,     '
         ' 1988).                                     '
         '                                            '
         '                                            '
         ' Degree of polarization, back azimuth and   '
         ' incidence estimates, as well as picks can  '
         ' be written into the SAC headers in the     '
         ' user and kuser variables.                  '
         '                                            '  
         '                                            '];
elseif index == 2
    hlpStr= ...                                               
        ['                                            '
         ' When BAZ starts, you are presented with the'
         ' main screen which consists initially of a  '
         ' menu and tool bar. There are three main    '
         ' menu items:                                '
         '                                            '
         '       File                                 '
         '       Action                               '
         '       Help                                 '
         '                                            '
         '                                            '
         ' The File menu allows you to move from one  '
         ' 3-component set of seismograms to another, '
         ' write results to the headers of those      '
         ' seismograms, and  quit the program.        ' 
         ' The Action menu allows you to set the band-'
         ' pass filter parameters, apply the filter to'
         ' the seismograms in memory, and view the    '
         ' polarization vs time plots.                '
         ' The Help menu provides this dialog. Most   '
         ' buttons on the toolbar are just shortcuts  '
         ' to items in the menu.                      '
         '                                            '
         '                                            '  
         ' When the BAZ module starts, the first set  '
         ' of 3-component seismograms is automatically'
         ' loaded and displayed. After processing this'
         ' set, you can move to the next set of seis- '
         ' mograms by clicking the Next button on the '
         ' tool bar, or by selecting Next from the    '
         ' File menu.                                 '
         '                                            '
         ' If there are any first arrival picks set in'
         ' header var. A , BAZ positions an analysis  '
         ' window starting at the mean of the pick    '
         ' times and continuing for 3 seconds. The    '
         ' location of the window is indicated on the '
         ' seismogram plots as a vertical bar with a  '
         ' "handle" <-> between the North and East    '
         ' traces. BAZ will compute the polarization  '
         ' of the signal within the window and display'
         ' the resulting back azimuth and incidence   '
         ' angle in a text box in the top-center part '
         ' of the window. Additionally, the back      '
         ' azimuth and incidence angles are displayed '
         ' schematically in a  pseudo-compass in the  '
         ' upper left part of the window and a pseudo-'
         ' dip meter in the upper right of the window.'
         ' The pseudo-compass also displays the       '
         ' orientation of the seismometer components  '
         ' as blue lines at right angles to each      '
         ' other. Back azimuth, incidence angle, and  '
         ' ellipticity (degree of polarization) are   '
         ' also displayed as text in the panel between'
         ' the pseudo-compass and pseudo-dip meter.   '
         ' These displays are automatically updated   '
         ' anytime an operation is performed that can '
         ' change the computed signal polarization.   '
         '                                            '
         '                                            '
         ' Use of the Mouse in Main Window            '
         '                                            '
         ' Besides selecting menu and toolbar items,  '
         ' the mouse can be used to zoom in or out on '
         ' the seismogram plot, move the analysis     '
         ' window, change the size of the analysis    '
         ' window, make and adjust picks, rotate      '
         ' seismometer component orientation, and     '
         ' change magnification of the seismograms.   '
         '                                            '
         ' To zoom in on the seismograms, move the    '
         ' mouse pointer to one edge of the area      '
         ' you wish to plot in the new view, and press'
         ' the left mouse button. While holding the   '
         ' button down, drag the mouse pointer to the '
         ' opposite edge of the area to be plotted. As'
         ' you drag, a dotted rectangle will outline  '
         ' the area being selected. When the desired  '
         ' area has been selected, release the left   '
         ' button, and the seismograms will be re-    '
         ' plotted in the new view. To reverse this   '
         ' action, click once anywhere in the plot    '
         ' with the right mouse button.               '
         '                                            '
         ' To move the analysis window, move the mouse'
         ' pointer over some part of the vertical bar '
         ' that marks the analysis window location.   '
         ' When you are over the bar, the pointer will'
         '  change shape from an arrow to a cross. At '
         ' that point press the left mouse button and '
         ' drag the window to the desired location.   '
         ' Then release the mouse button.             '
         '                                            '
         ' To change the size of the analysis window, '
         ' move the mouse pointer over the "handle" to'
         ' the window (<->). Press the left button,   '
         ' and drag the handle until the window is the'
         ' desired width.                             '
         '                                            '
         ' The traces can be magnified or de-magnified'
         ' using the scroll bar to the right of the   '
         ' seismograms. The nominal magnification is  '
         ' such that the traces are scaled to just    '
         ' fit in their plotting regions. Using the   '
         ' scroll bar the magnification can be        '
         ' increased to 10 or decreased to 0.1.       '
         '                                            '
         ' To rotate seismometer components click with'
         ' the mouse on one of the axis symbols in the'
         ' compass window, and drag the axis to the   '
         ' desired orientation. When the mouse button '
         ' is released, the rotation is performed.    '
         ' The new component azimuths are displayed on'
         ' the main plot after the station name. You  '
         ' can also rotate the components to the      '
         ' current back azimuth estimate by clicking  '
         ' on the Rotate to BAZ button.               '
         '                                            '
         '  The mouse can also be used to add or edit '
         ' picks. To add a pick, press and hold the   '
         ' center mouse button. A cross-hair will     '
         ' appear, and you can drag it to the desired '
         ' location and then release the mouse button.'
         ' When you release the mouse button the pick '
         ' will be labeled and a drop-down list of    '
         ' pick types will appear at the cursor       '
         ' location. If the labeled pick is not one   '
         ' you desire, open the list with the left    '
         ' mouse button, and select the correct pick  '
         ' type. Otherwise, just move the pointer away'
         ' from the list, and the list will go away.  '
         ' You can set multiple picks on a single     '
         ' seismogram. However, you cannot set more   '
         ' than one pick of the same type on the same '
         ' trace. If you do so, the latest pick of    '
         ' that type will replace any earlier pick.   '
         ' Existing picks can be moved by clicking on '
         ' the pick label with the left mouse button  '
         ' and dragging to the new location. Any pick '
         ' can be changed to a new type by first      '
         ' selecting it (click once on the pick label)'
         ' and then choosing the new pick type from   '
         ' the  drop-down list near the top-center of '
         ' the screen. Picks can also be deleted by   '
         ' first selecting them and then either       '
         ' clicking the delete button, or pressing the'
         ' "d" key on the keyboard. Be aware that the '
         ' last pick that was modified is always      '
         ' selected. (You can tell which pick this is '
         ' because its label will be in red instead of'
         ' black.) Thus, delete actions, or pick-type '
         ' changes can be made without specifically   '
         ' selecting a pick.                          '
         '                                            '];
         
elseif index == 3
    hlpStr= ...                                               
        ['                                            '
         ' When seismograms are read into BAZ, the    '
         ' filter is set so that the lower corner is  '
         ' at a frequency of 0.001 Hz and the upper   '
         ' corner is at the Nyquist frequency. Thus,  '
         ' the default filtering action is to do      '
         ' nothing. To modify this, select "Adjust    '
         ' Filter" from the Action menu. This opens   '
         ' the "Adjust Filter Parameters" window. This'
         ' window displays a plot of the filter       '
         ' response in log-lin space. The two corner  '
         ' frequencies are shown on the plot as       '
         ' blue and red lines. The frequencies can be '
         ' adjusted by dragging a corner frequency    '
         ' line to a desired frequency. Alternatively,'
         ' the frequencies can be entered into edit   '
         ' boxes in the dialog on the right-hand side '
         ' of the window. The dialog also provides an '
         ' edit box to change the number of poles in  '
         ' the filter.                                '  
         '                                            '  
         '                                            '];

elseif index == 4
    hlpStr= ...                                               
        ['                                            '
         ' BAZ also provides a way to view particle   '
         ' motion in three dimensions. This may be    '
         ' used to check the polarization determined  '
         ' automatically, or simply to gain insight   '
         ' into contributions of scattered arrivals in'
         ' the seismograms. To view particle motion,  '
         ' select "PPM" from the toolbar.             '
         '                                            '  
         ' This will open the "Particle Motion Plot"  '
         ' window. At the top of this window is a plot'
         ' of the Z-component of motion for the       '
         ' current analysis window. At the bottom of  '
         ' the screen is a three-dimensional particle '
         ' motion plot. The initial viewing angle for '
         ' particle motion plots is an azimuth of 20  '
         ' degrees east of South and 30 degrees above '
         ' the horizon. Notice that the color of the  '
         ' seismogram in the upper plot grades from   '
         ' light cyan at the beginning to magenta at  '
         ' the end. This same color gradation is      '
         ' applied to the particle motion plot, and   '
         ' provides one clue as to the direction of   '
         ' particle motion. Also, the thickness of the'
         ' line in the particle motion plot increases '
         ' continuously with duration, providing      '
         ' another clue as to particle direction. As a'
         ' final indicator of particle motion, BAZ    '
         ' displays 3-D arrow heads at intervals along'
         ' the particle trajectory.                   '
         '                                            '  
         ' You can change the viewing angle of the    '
         ' particle motion plot in two ways. The first'
         ' way is to type the desired viewing angle   '
         ' into the edit boxes in the "Viewing Angles"'
         ' dialog. Alternatively, you can use the     '
         ' mouse to adjust viewing angles. To rotate  '
         ' the plot horizontally, locate the letter   '
         ' "E" at the end of the east axis on the     '
         ' particle motion plot. Click on  "E" with   '
         ' the left mouse button to rotate clockwise  '
         ' (viewed from top), or with the right mouse '
         ' button to rotate counter clockwise. To     '
         ' rotate vertically, click on the "Z" label  '
         ' with the left or right mouse buttons.      ' 
         '                                            '  
         ' Changes to the analysis window made in the '
         ' main screen are reflected immediately in   '
         ' the particle motion plot. Also, any        '
         ' filtering actions cause an immediate update'
         ' of the particle motion plot.               '
         '                                            '  
         '                                            '  
         '                                            '];
         
elseif index == 5
    hlpStr= ...                                               
        ['                                            '
         ' The polarization module of the BAZ program '
         ' allows calculation of wavefield polariz-   '
         ' ation using a maximum likelihood estimation'
         ' technique by Christoffersson et al. 1988.  '
         ' In this technique, a model covariance      '
         ' matrix is constructed for a specific       '
         ' wavetype and is compared to the data covar-'
         ' iance matrix after rotation in azimuth. A  '
         ' scalar measure of the misfit is computed,  '
         ' and compared to the misfit between the     '
         ' wavetype model and a noise model using Chi-'
         ' squared statistics. This allows mapping the'
         ' probability of a specific wavetype as a    '
         ' function of time and azimuth. The resulting'
         ' plot is reminiscent of plots produced using'
         ' array analysis methods. Currently, BAZ is  '
         ' restricted to modeling P-wave and SH-wave  '
         ' arrivals.                                  '
         '                                            '
         ' To use the polarization module, first      '
         ' select the portion of the seismograms to   '
         ' analyze with the analysis window (See "Main'
         ' Screen" for instructions). Then click on   '
         ' the "Polarization" button on the tool bar  '
         ' or select "Polarization" from the "Action" '
         ' menu. This opens the "Polarization Results"'
         ' window. The tool bar at the top of this    '
         ' window contains controls for setting grid- '
         ' search parameters, and for modifying and   '
         ' printing the probability plots.            '
         '                                            '
         ' The "Win-Len" edit control is used to set  '
         ' the length (in seconds) of the moving      '
         ' window. Adjustment of the window has only a'
         ' minor effect on execution time, but it has '
         ' a significant effect on both resolution and'
         ' stability of the estimates. If the window  '
         ' is too short, you are likely to get large  '
         ' numbers of spurious peaks in probability   '
         ' because of chance agreement of very short  '
         ' segments of the waveform with the model    '
         ' being tested. On the other hand, if the    '
         ' window is very long relative to the        '
         ' duration of the arrivals being tested, both'
         ' temporal and azimuthal resolution will be  '
         ' degraded.                                  '
         '                                            '
         ' The "Time Step" control is used to set the '
         ' increment in the window position between   '
         ' successive estimates. Obviously, adjustment'
         ' of the time step will affect temporal      '
         ' resolution. The execution time for the grid'
         ' search is inversely related to the size of '
         ' the time step, so, although you can improve'
         ' temporal resolution by decreasing the time '
         ' step, this may result in very lengthy      '
         ' calculations.                              '
         '                                            '
         ' The "Azimuth Step" is the increment in     '
         ' degrees between successive azimuth         '
         ' estimates. Like the "Time Step" its size is'
         ' inversely related to execution time, so if '
         ' both the "Time Step" and the "Azimuth Step"'
         ' are set to very small values, it may take a'
         ' long time to produce the polarization plot.'
         '                                            '
         ' At each point in time-azimuth space, the   '
         ' model covariance matrix is constructed for '
         ' a range of incidence angles, and the       '
         ' highest probability is returned. The       '
         ' "Incidence Start", "Incidence Step", and   '
         ' "Incidence Stop" are the parameters that   '
         ' control the search in incidence angle.     '
         ' Obviously, combinations that produce       '
         ' evaluations at many incidence angles will  '
         ' also increase execution time.              '
         '                                            '
         ' When the controls are set as you like,     '
         ' click on the "Calculate" button to start   '
         ' the grid search. BAZ will display a        '
         ' progress indicator to inform you of the    '
         ' status of the calculations. When the       '
         ' calculations are complete, a color         '
         ' probability plot will be displayed in the  '
         ' lower half of the window. The Z-component  '
         ' seismogram is superimposed on the plot as a'
         ' reference. The default colormap for the    '
         ' plot is called "hot", and in this map,     '
         ' black corresponds to 0 probability, and    '
         ' white corresponds to a probability of 1.0. '
         ' You can change the colormap using the drop-'
         ' down list next to the "Calculate" button.  '
         ' This can be done before or after generation'
         ' of the probability plot. You can also      '
         ' smooth the probability plot by choosing    '
         ' "interp" from the drop-down list next to   '
         ' the "Print" button. Be aware, however, that'
         ' interpolated plots may take a very long    '
         ' time to print. For printing purposes, it   '
         ' may be quicker to choose a small time step '
         ' and small azimuth step and use flat shading'
         ' than to print an interpolated probability  '
         ' plot.                                      '
         '                                            '
         ' Plots can be printed by clicking the       '
         ' "Print" button. This opens a print dialog  '
         ' allowing you to choose between sending the '
         ' plot directly to a printer or saving the   '
         ' plot in either Illustrator or Postscript   '
         ' format. You set the destination printer    '
         ' with the edit box next to the "Printer"    '
         ' button in the print dialog. To send the    '
         ' plot to a printer other than your default  '
         ' printer, type the desired printer name in  '
         ' the box.                                   '
         '                                            '  
         '                                            '  
         '                                            '];

end


if isempty( findobj(h7,'Tag','HelpBorder') )
   line([0 0 1 1 0],[0 3 3 0 0],'color','k','LineWidth',3,'Tag','HelpBorder')
end

if isempty( findobj(h7,'Tag','HelpSlider') )
   uicontrol('style','slider','position',[338 20 15 360],'Tag',...
   'HelpSlider','visible','off','callback','ScrollHelp')
end

[M,N]=size(hlpStr);
for j=1:M
   ypos=(j-1)*.14;
   string=hlpStr(j,:);
   h(j)=text(0.01,ypos,string,'color','k','visible','off','Tag','hlpStr',...
        'fontsize',12);
end
if ypos > 3.0
   hs = findobj(h7,'Tag','HelpSlider');
   set(hs,'max',ypos,'value',ypos)
   set(hs,'visible','on')
else
   set(findobj(h7,'Tag','HelpSlider') ,'visible','off')
end

for j=1:M
   pos=get(h(j),'position');
   if pos(2) < 3,set(h(j),'visible','on'),end
end
