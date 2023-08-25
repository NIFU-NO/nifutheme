%% Implemented by Martin Helso (helsomartin@gmail.com)


\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{NIFU}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              Class options                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Language:
\DeclareOption{american}
{
    \PassOptionsToPackage{main = american}{babel}
    \def\NIFU@lang{eng}
    \pretolerance = 1000
    \tolerance    = 2000
}
\DeclareOption{british}
{
    \PassOptionsToPackage{main = british}{babel}
    \def\NIFU@lang{eng}
    \pretolerance = 1000
    \tolerance    = 2000
}
\DeclareOption{english}
{
    \PassOptionsToPackage{main = english}{babel}
    \def\NIFU@lang{eng}
    \pretolerance = 1000
    \tolerance    = 2000
}
\DeclareOption{UKenglish}
{
    \PassOptionsToPackage{main = UKenglish}{babel}
    \def\NIFU@lang{eng}
    \pretolerance = 1000
    \tolerance    = 2000
}
\DeclareOption{USenglish}
{
    \PassOptionsToPackage{main = USenglish}{babel}
    \def\NIFU@lang{eng}
    \pretolerance = 1000
    \tolerance    = 2000
}
\DeclareOption{norsk}
{
    \def\NIFU@lang{no}
    \pretolerance = 2000
    \tolerance    = 6000
}


%% Document type
\DeclareOption{report}
{
    \def\NIFU@documenttype{\csname NIFU@report@\NIFU@lang\endcsname}
    \def\NIFU@documentcolour{NIFURed}
}
\DeclareOption{WorkingPaper}
{
    \def\NIFU@documenttype{\csname NIFU@workingpaper@\NIFU@lang\endcsname}
    \def\NIFU@documentcolour{NIFUAqua}
}


\ExecuteOptions{norsk}
\ExecuteOptions{report}
\ProcessOptions*


\LoadClass[11pt,
           openany,
           extrafontsizes
           oldfontcommands]{memoir}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                 Packages                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Encoding
%\RequirePackage[utf8]{inputenx} % Fjernet av Stephan fordi Quarto legger også inn
\RequirePackage[T1]{fontenc}


%% Fonts and typography
\RequirePackage[sc]{mathpazo}     % URW Palladio
\RequirePackage[lf]{carlito}      % Carlito
\RequirePackage[scaled]{beramono} % Bera Mono (Bitstream Vera Sans Mono)
\RequirePackage[final]{microtype}
\setlength{\emergencystretch}{1em}
\linespread{1.05}


%% Language
\RequirePackage[norsk]{babel}
\RequirePackage{csquotes}

%% Graphics
\RequirePackage{graphicx}

%% Lists
\RequirePackage{enumitem}
\setlist[itemize]
{
    font   = \upshape,
    before = \leavevmode
}
\setlist[itemize, 1]{label = \textbullet}
\setlist[itemize, 2]{label = \(\circ\)}
\setlist[itemize, 3]{label = \(\sqbullet\)}
\setlist[itemize, 4]{label = \textbullet}
\setlist[enumerate]
{
    font   = \upshape,
    before = \leavevmode
}
\setlist[enumerate, 1]
{
    label  = \arabic{*}.,
    ref    = \arabic{*}
}
\setlist[enumerate, 2]
{
    label  = \alph{*}.,
    ref    = \arabic{enumi}.\alph{*}
}
\setlist[enumerate, 3]
{
    label  = \roman{*}.,
    ref    = \arabic{enumi}.\alph{enumii}.\roman{*}}
\setlist[enumerate, 4]
{
    label  = \arabic{*}.,
    ref    = \arabic{enumi}.\alph{enumii}.\roman{enumiii}.\arabic{*}
}
\setlist[description]
{
    font   = \sffamily,
    before = \leavevmode
}


%% Symbol for square bullet
\DeclareFontFamily{U}{mathb}{\hyphenchar\font45}
\DeclareFontShape{U}{mathb}{m}{n}
{
    <5> <6> <7> <8> <9> <10>
    <10.95> <12> <14.4> <17.28> <20.74> <24.88>
    mathb10
}{}
\DeclareSymbolFont{mathb}{U}{mathb}{m}{n}
\DeclareMathSymbol{\sqbullet}{1}{mathb}{"0D}


%% Graphics
\RequirePackage{graphicx}
\RequirePackage{eso-pic}
\RequirePackage{calc}
\RequirePackage{xcolor}
\definecolor{MicrosoftBlue}{RGB}{0, 112, 192}
\definecolor{NIFUAqua}{RGB}{45, 142, 159}
\definecolor{NIFUBlueGray}{RGB}{68, 84, 106}
\definecolor{NIFUDarkGray}{RGB}{54, 54, 54}
\definecolor{NIFULightGray}{RGB}{231, 230, 230}
\definecolor{NIFULavender}{RGB}{219, 210, 224}
\definecolor{NIFUTan}{RGB}{237, 226, 210}
\definecolor{NIFUOrange}{RGB}{232, 174, 99}
\definecolor{NIFURed}{RGB}{200, 73, 87}


%% Tools for package writing
\RequirePackage{etoolbox}


%% Cross-references and hyperlinks
\RequirePackage{varioref}
\RequirePackage[pdfusetitle,
                colorlinks,
                allcolors =,
                urlcolor  = MicrosoftBlue]
                {hyperref}
\RequirePackage{amsmath} %% Lagt til av Stephan for å unngå cleveref must be loaded after amsmath				
\RequirePackage[nameinlink, noabbrev]{cleveref}
\urlstyle{same}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  Layout                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Paper size
\pageaiv
\stockaiv
\setlrmarginsandblock{41mm}{41mm}{*}
\setulmarginsandblock{32mm}{34mm}{*}
\checkandfixthelayout


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Sections %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Numbering depth
\maxtocdepth{subsection}
\setsecnumdepth{subsection}


%% Hanging numbers
\newlength{\secspacer}
\setlength{\secspacer}{20pt}
\chapterstyle{hangnum}
\renewcommand*{\printchapternum}
{\noindent\llap{\chapnumfont\thechapter\kern\secspacer}}
\renewcommand{\hangsecnum}
{%
    \def\@seccntformat##1{\llap{\csname the##1\endcsname\kern\secspacer}}
}
\setsechook{\hangsecnum}
\setsubsechook{\hangsecnum}


%% Section heading fonts
\renewcommand*{\chapnumfont}{\Huge\sffamily\bfseries}
\renewcommand*{\chaptitlefont}{\Huge\sffamily\bfseries\raggedright}
\setsecheadstyle{\LARGE\sffamily\bfseries\raggedright}
\setsubsecheadstyle{\Large\sffamily\bfseries\raggedright}
\setsubsubsecheadstyle{\large\sffamily\raggedright}
\setparaheadstyle{\large\sffamily\itshape\raggedright}
\setsubparaheadstyle{\normalsize\sffamily\itshape\raggedright}


%% Section spacing
\addtolength{\beforechapskip}{-1.05\baselineskip}
\addtolength{\afterchapskip}{1.90\baselineskip}
\setafterparaskip{1.5ex plus 0.2ex}
\setbeforeparaskip{-\beforeparaskip}
\setsubparaindent{0pt}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Title page %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\AtBeginDocument
{
	\cover
    \maketitle
    \colophon
	\preface
	\tableofcontents* 
    \clearpage
}


\newcommand{\cover}
{
    \thispagestyle{empty}

    \AddToShipoutPictureBG*{%
        \AtPageLowerLeft{%
            \includegraphics[width=\paperwidth, height=\paperheight]{../NIFU_graphic/cover_nedre.png}%
        }%
    }

    \newlength{\imageheight}
    \settoheight{\imageheight}{\includegraphics{\topImagePath}}


    \AddToShipoutPictureBG*{%
        \AtPageUpperLeft{%
            \put(0,-1.64\imageheight){\includegraphics[width=\paperwidth, height=\paperheight]{../NIFU_graphic/cover_ovre.png}}%
        }%
    }
    \begin{adjustwidth}{-16mm}{0mm}
        \sffamily
        \vspace*{100mm}
        \begin{flushright}
            \large
            \NIFU@documenttype
            \par
            \NIFU@report
        \end{flushright}
		

        \begin{flushleft}
            \vspace{15mm}
            \parbox{130mm}
            {
                \Huge
                \bfseries
                \raggedright
                \thetitle

                \vspace*{5mm}

                \Large
                \mdseries
                \thesubtitle
            }

            \vspace*{\fill}
            \par
            \textcolor{\NIFU@documentcolour}{\rule{21mm}{1.4bp}}
            \vspace*{5mm}

            \parbox{130mm}
            {
                \large
                \raggedright
                \theauthor
            }

            \vspace*{17mm}
        \end{flushleft}
	
    \end{adjustwidth}
	\newpage
	\thispagestyle{empty}
	\mbox{}
	\newpage

}

\renewcommand{\maketitle}
{
    \begin{adjustwidth}{-16mm}{0mm}
        \thispagestyle{empty}
        \sffamily
        \vspace*{100mm}
        \begin{flushright}
            \large
            \NIFU@documenttype
            \par
            \NIFU@report
        \end{flushright}

        \begin{flushleft}
            \vspace{15mm}
            \parbox{130mm}
            {
                \Huge
                \bfseries
                \raggedright
                \thetitle

                \vspace*{5mm}

                \Large
                \mdseries
                \thesubtitle
            }

            \vspace*{\fill}
            \par
            \textcolor{\NIFU@documentcolour}{\rule{21mm}{1.4bp}}
            \vspace*{5mm}

            \parbox{130mm}
            {
                \large
                \raggedright
                \theauthor
            }

            \vspace*{17mm}
        \end{flushleft}
    \end{adjustwidth}
    \clearpage

}


\newcommand{\subtitle}{\def\thesubtitle}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Colophon %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\newcommand{\colophon}
{{
    \thispagestyle{empty}
    \footnotesize
    \noindent
    \makebox[\textwidth]
    {
        \begin{tabular}{p{0.2\textwidth}p{0.8\textwidth}}
            \NIFU@documenttype
            &
            \NIFU@report
            \tabularnewline[1em]
            \csname NIFU@publisher@\NIFU@lang\endcsname
            &
            \NIFU@thepublisher
            \tabularnewline
            \csname NIFU@address@\NIFU@lang\endcsname
            &
            \NIFU@publisheraddress
            \tabularnewline[1em]
            \csname NIFU@projectno@\NIFU@lang\endcsname
            &
            \NIFU@theprojectno
            \tabularnewline[1em]
            \csname NIFU@customer@\NIFU@lang\endcsname
            &
            \NIFU@thecustomer
            \tabularnewline
            \csname NIFU@address@\NIFU@lang\endcsname
            &
            \NIFU@customeraddress
            \tabularnewline[1em]
            \csname NIFU@photo@\NIFU@lang\endcsname
            &
            \NIFU@thephoto
            \tabularnewline[1em]
            ISBN
            &
            \NIFU@ISBN
            \tabularnewline
            ISSN
            &
            \NIFU@ISSN
            \tabularnewline[1em]
            \includegraphics[width = \linewidth]{NIFU-CC-BY}
            \tabularnewline[0.5ex]
            \multicolumn{2}{l}{Copyright NIFU: CC BY 4.0}
            \tabularnewline[2em]
            \url{www.nifu.no}
       \end{tabular}
    }
    \clearpage

}}

\newcommand{\preface}
{{
	\chapter*{\csname NIFU@prefacetext@\NIFU@lang\endcsname}
    \NIFU@prefacetext
	\\
	\\	
	\oslo
	\\
	\\
	\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}ll@{}}
		\NIFU@prefaceAuthorA
		&
		\NIFU@prefaceAuthorB
		\\
		\NIFU@prefaceAuthorAposition
		&
		\NIFU@prefaceAuthorBposition
	\end{tabular*}
	\setcounter{page}{3}
	\newpage
	\mbox{}
	\newpage
}}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Last page %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\AtEndDocument
{
    \clearpage
    \lastpage
}


\newcommand{\lastpage}
{
    \thispagestyle{empty}
    \vspace*{\fill}
    \begin{flushleft}
        \scriptsize
        Nordisk institutt for studier av\par
        innovasjon, forskning og utdanning

        \medskip

        Nordic Institute for Studies in\par
        Innovation, Research and Education

        \bigskip

        \url{www.nifu.no}
    \end{flushleft}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Footer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\makepagestyle{NIFU}
\makeoddfoot{NIFU}{}
{
    \sffamily
    \small
    \thepage
    \space
    \space
    \textcolor{\NIFU@documentcolour}{\textbullet}%
    \space
    \space
    \scriptsize
    \NIFU@documenttype
    \space
    \NIFU@report
}{}
\makeevenfoot{NIFU}{}
{
    \sffamily
    \small
    \thepage
    \space
    \space
    \textcolor{\NIFU@documentcolour}{\textbullet}%
    \space
    \space
    \scriptsize
    \NIFU@documenttype
    \space
    \NIFU@report
}{}
\pagestyle{NIFU}
\aliaspagestyle{chapter}{NIFU}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Captions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\captionnamefont{\sffamily\bfseries}
\captiontitlefont{\sffamily\bfseries}
\captionstyle{\raggedright}
\AtBeginEnvironment{figure}{\setlength{\abovecaptionskip}{-5pt}}


\newcommand{\source}[1]
{
    \captiontitlefinal
    {
        \footnotesize
        \sffamily
        \mdseries
        \itshape
        \\[1ex]
        \csname NIFU@source@\NIFU@lang\endcsname:
        #1
    }
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Quotes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\AtBeginEnvironment{quote}{\itshape}
\AtBeginEnvironment{quotation}{\itshape}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Language macros                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Automatic translations
\newcommand{\NIFU@report@no}{Rapport}
\newcommand{\NIFU@report@eng}{Report}
\newcommand{\NIFU@workingpaper@no}{Arbeidsnotat}
\newcommand{\NIFU@workingpaper@eng}{Working paper}
\newcommand{\NIFU@publisher@no}{Utgitt av}
\newcommand{\NIFU@publisher@eng}{Published by}
\newcommand{\NIFU@thepublisher@no}
{Nordisk institutt for studier av innovasjon, forskning og utdanning (NIFU)}
\newcommand{\NIFU@thepublisher@eng}
{Nordic Institute for Studies in Innovation, Research and Education (NIFU)}
\newcommand{\NIFU@address@no}{Adresse}
\newcommand{\NIFU@address@eng}{Address}
\newcommand{\NIFU@theaddress@no}
{
    Postboks 2815 T{\o}yen,
    0608 Oslo.
    Bes{\o}ksadresse:
    {\O}kernveien 9,
    0653 Oslo.
}
\newcommand{\NIFU@theaddress@eng}
{
    P.O.\ Box 2815 T{\o}yen,
    N-0608 Oslo.\par
    Visiting address:
    {\O}kernveien 9,
    N-0653 Oslo.
}
\newcommand{\NIFU@projectno@no}{Prosjektnr.}
\newcommand{\NIFU@projectno@eng}{Project No.}
\newcommand{\NIFU@customer@no}{Oppdragsgiver}
\newcommand{\NIFU@customer@eng}{Customer}
\newcommand{\NIFU@photo@no}{Fotomontasje}
\newcommand{\NIFU@photo@eng}{Photo montage}
\newcommand{\NIFU@source@no}{Kilde}
\newcommand{\NIFU@source@eng}{Source}
\newcommand{\NIFU@bibname@no}{Referanser}
\newcommand{\NIFU@bibname@eng}{References}
\newcommand{\NIFU@prefacetext@no}{Forord}
\newcommand{\NIFU@prefacetext@eng}{Preface}


%% Colophon data
\newcommand{\report}[1]{\def\NIFU@report{#1}}
\newcommand{\workingpaper}[1]{\def\NIFU@report{#1}}
\newcommand{\publisher}[1]{\def\NIFU@thepublisher{#1}}
\newcommand{\publisheraddress}[1]{\def\NIFU@publisheraddress{#1}}
\newcommand{\projectno}[1]{\def\NIFU@theprojectno{#1}}
\newcommand{\customer}[1]{\def\NIFU@thecustomer{#1}}
\newcommand{\customeraddress}[1]{\def\NIFU@customeraddress{#1}}
\newcommand{\photo}[1]{\def\NIFU@thephoto{#1}}
\newcommand{\ISBN}[1]{\def\NIFU@ISBN{#1}}
\newcommand{\ISSN}[1]{\def\NIFU@ISSN{#1}}
\newcommand{\prefacetext}[1]{\def\NIFU@prefacetext{#1}}
\newcommand{\prefaceAuthorA}[1]{\def\NIFU@prefaceAuthorA{#1}}
\newcommand{\prefaceAuthorAposition}[1]{\def\NIFU@prefaceAuthorAposition{#1}}
\newcommand{\prefaceAuthorB}[1]{\def\NIFU@prefaceAuthorB{#1}}
\newcommand{\prefaceAuthorBposition}[1]{\def\NIFU@prefaceAuthorBposition{#1}}

%% Default values
\title{}
\subtitle{}
\author{}
\report{}
\publisher{\csname NIFU@thepublisher@\NIFU@lang\endcsname}
\publisheraddress{\csname NIFU@theaddress@\NIFU@lang\endcsname}
\projectno{}
\customer{}
\customeraddress{}
\photo{}
\ISBN{}
\ISSN{1892-2597 (online)}
\prefacetext{Vi takker oppdragsgiver, kvalitetssikrere og informanter for gode innspill.}
\prefaceAuthorA{Vibeke Opheim}
\prefaceAuthorAposition{Direkt\o r}
\prefaceAuthorB{}
\prefaceAuthorBposition{Forskningsleder}

%% Rename autogenerated chapter titles
\addto\captionsamerican
{
    \renewcommand{\listfigurename}{List of figures}
    \renewcommand{\listtablename}{List of tables}
}
\addto\captionsbritish
{
    \renewcommand{\listfigurename}{List of figures}
    \renewcommand{\listtablename}{List of tables}
}
\addto\captionsenglish
{
    \renewcommand{\listfigurename}{List of figures}
    \renewcommand{\listtablename}{List of tables}
}
\addto\captionsUKenglish
{
    \renewcommand{\listfigurename}{List of figures}
    \renewcommand{\listtablename}{List of tables}
}
\addto\captionsUSenglish
{
    \renewcommand{\listfigurename}{List of figures}
    \renewcommand{\listtablename}{List of tables}
}
\addto\captionsnorsk
{
    \renewcommand{\listfigurename}{Figuroversikt}
    \renewcommand{\listtablename}{Tabelloversikt}
}
\AfterPreamble
{
    \renewcommand{\bibname}{\csname NIFU@bibname@\NIFU@lang\endcsname}
}


%% Print "Oslo, date"
\newcommand{\oslo}
{
    \par
    \bigskip
    \noindent
    Oslo,
    \today
    \par
    \bigskip
    \noindent
}


\endinput