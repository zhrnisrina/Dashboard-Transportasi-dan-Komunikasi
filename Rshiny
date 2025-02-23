setwd("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/SIM/FP SIM")

library(shiny)
library(shinydashboard)
library(summarytools)
library(readxl)
library(DT)
library(plotly)
library(ggplot2)
library(rhandsontable)
library(datasets)
library(fontawesome)
library("readxl")
library(ggplot2)
library("tuneR")
library("markdown")
library(scales)
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet)
library(DT)
library(highcharter)
library(shinycssloaders)


data1 <- read_excel("Rshiny_FaZa.xlsx", sheet = 1)  ;data1
data2 <- read_excel("Rshiny_FaZa.xlsx", sheet = 2)  ;data2
data3 <- read_excel("Rshiny_FaZa.xlsx", sheet = 3)  ;data3
data4 <- read_excel("Rshiny_FaZa.xlsx", sheet = 4)  ;data4
data5 <- read_excel("Rshiny_FaZa.xlsx", sheet = 5)  ;data5
data6 <- read_excel("Rshiny_FaZa.xlsx", sheet = 6)  ;data6
data7 <- read_excel("Rshiny_FaZa.xlsx", sheet = 7)  ;data7
data8 <- read_excel("Rshiny_FaZa.xlsx", sheet = 8)  ;data8
data9 <- read_excel("Rshiny_FaZa.xlsx", sheet = 13)  ;data9
data9 <- read_excel("Rshiny_FaZa.xlsx", sheet = 13)  ;data9
data82 <- read_excel("Rshiny_FaZa.xlsx", sheet = 9)  ;data82
data72 <- read_excel("Rshiny_FaZa.xlsx", sheet = 10)  ;data72
bb_data <- read_excel("Rshiny_FaZa.xlsx", sheet = 12) ;bb_data
bb_data <- data.frame(bb_data)
bb_data$lat <-  as.numeric(bb_data$lat)
bb_data$long <-  as.numeric(bb_data$long)

data9$No. <- as.factor(data9$No.)
data1 <- data1[-c(39,78,117), ]
data8$Bulan <- c(1,2,3,4,5,6,7,8,9,10,11,12)

#KONDISI JALAN

Kondisi <- c("Baik", "Sedang", "Rusak", "Rusak Berat")
Proporsi <- c(820.44/1421.01, 452.92/1421.01, 101.37/1421.01, 46.28/1421.01)
df <- data.frame(Kondisi, Proporsi) ; df

jalan <- ggplot(df, aes(x="", y=Proporsi, fill=Kondisi)) +scale_fill_brewer(palette="Purples") +
  geom_bar(stat="identity", width=1) + xlab("") + ylab("") + theme_void() +
  coord_polar("y", start=0)

#JUMLAH PENUMPANG KENDARAAN 

data6[99 , c(2,4,6,8,10,12,14,16,18,20,22,24)]
penumpang <- c(453960,477200,631139,675361,708151,799257,138594,115649,221649,625728,818458,1020068)
month <- c(01,02,03,04,05,06,07,08,09,10,11,12)
naikkereta <- data.frame(month, penumpang)

#KERETA
a <- ggplot(naikkereta, aes(x=month, y=penumpang, group = 1, label = penumpang)) + geom_line(color = "#590696", size = 1.3) + geom_point(color = "#371B58", size = 2) +
  ggtitle("Jumlah Penumpang Naik Kereta Api (2019)") + labs(y="Jumlah Penumpang Naik (dalam ribuan) \n") + labs(x="Bulan") + theme(
    plot.title = element_text(color = "#9A86A4", size = 20, face = "bold.italic"), panel.background = element_rect(fill = "#F7F5F2")
  ) + scale_x_continuous(limits = c(0, 12), breaks = c(01,02,03,04,05,06,07,08,09,10,11,12)) +
  geom_text(nudge_y = 45, color = "#2E0249") 

#PESAWAT

ggplot(data8, aes(x=month, y=`2017`, group = 1)) + geom_line(color = "#5B7DB1", size = 1.3) + geom_point(color = "#5B7DB1", size = 3) +
  ggtitle("Jumlah Penumpang Pesawat Udara (2017)") + labs(y="Jumlah Penumpang ") + labs(x="Bulan") + theme(
    plot.title = element_text(color = "#5B7DB1", size = 22, face = "bold.italic"), panel.background = element_rect(fill = "#F7F5F2")
  ) + scale_x_continuous(limits = c(0, 12), breaks = c(01,02,03,04,05,06,07,08,09,10,11,12))

b <- ggplot(data82, aes(x=Tahun, y=Januari, group = 1)) + geom_line(color = "#5B7DB1", size = 1.3) + geom_point(color = "#5B7DB1", size = 3) +
  ggtitle("Jumlah Penumpang Pesawat Udara") + labs(y="Jumlah Penumpang ") + labs(x="Tahun") + theme(
    plot.title = element_text(color = "#5B7DB1", size = 22, face = "bold.italic"),
    panel.background = element_rect(fill = "#F7F5F2"))


# HOME PAGE

videos <- data.frame(
  welcome = c("Welcome", "Music", "Thankyou"),
  id = c("jLViunHSFVU", "SqS7Mr9Vuk0", "holiQzhGkos")
)

#DASHBOARD

ui <- dashboardPage(
  dashboardHeader(title = "~ FaZa ~"
                  , titleWidth = 300,
                  dropdownMenu( type = 'message',
                                icon = icon("share-alt"),
                                messageItem(
                                  from = "Putri Nisrina Az-Zahra",
                                  message = "LinkedIn",
                                  icon = icon("linkedin"),
                                  href = "https://www.linkedin.com/in/putri-nisrina-az-zahra-188793231/"
                                ),
                                messageItem(
                                  from = "",
                                  message = "Instagram",
                                  icon = icon("instagram"),
                                  href = "https://www.instagram.com/zhrnisrina/"
                                ),
                                messageItem(
                                  from = 'Farsya Riyanti Azzahra',
                                  message = "LinkedIn",
                                  icon = icon("linkedin"),
                                  href = "https://www.linkedin.com/in/farsya-riyanti-azzahra-98362323b/"
                                ),
                                messageItem(
                                  from = "",
                                  message = "Instagram",
                                  icon = icon("instagram"),
                                  href = "https://www.instagram.com/farsyaraz/"
                                ))),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home",tabName = "Home", icon=icon("home")),
      menuItem("Profil Jawa Timur",tabName = "Peta", icon=icon("leaf")),
      menuItem("Transportasi",tabName = "Transportasi", icon=icon("car")),
      menuItem("Komunikasi",tabName="Komunikasi", icon=icon("phone")),
      menuItem("Dataset Transportasi",tabname = "Transportasi_2", icon=icon("database"),
               menuSubItem("Panjang Jalan",tabName = "Transportasi_3"),
               menuSubItem("Kendaraan Bermotor",tabName = "Transportasi_4"),
               menuSubItem("Jumlah Penumpang Kendaraan",tabName = "Transportasi_5")
      ),
      menuItem("Dataset Komunikasi",tabname = "Komunikasi_2", icon=icon("database"),
               menuSubItem("Jumlah Kantor Pos",tabName = "Komunikasi_3"),
               menuSubItem("Banyak Desa (Penerimaan Sinyal Internet)",tabName = "Komunikasi_4")
      ),
      menuItem("Get to Know", tabName = "Get", icon=icon("question"))
    ),
    tags$head(tags$style(HTML('* {font-family: "Montserrat"};')))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              selectInput("welcome", "Choose What to Play", c("Welcome", "Music", "Thankyou")),
              uiOutput("video") %>% withSpinner(color = "purple")
      ),
      tabItem(tabName = "Peta",
              fluidRow(
                box(width = 12,
                    div(strong("Peta Jawa Timur beserta Data Transportasi dan Komunikasi"),style="text-align: center;font-size: 160%"),
                ),
                br(),
                box(width = 12,
                    leafletOutput("bbmap", height=700) %>% withSpinner(color = "purple"))
              )
      ),
      tabItem(tabName = "Get",
              fluidRow(
                box(width = 12,
                    div(strong("Anggota Kelompok"),style="text-align: center;font size: 160%"))),
              box(width = 6,
                  status = NULL,
                  div(imageOutput("farsya"),style="text-align: center;",
                      style = "margin-bottom:-180px;"),
                  div(strong("Farsya Riyanti Azzahra"),style="text-align: center;"),
                  div(strong("5003201134"),style="text-align: center;")
              ),
              box(width = 6,
                  status = NULL,
                  div(imageOutput("zahra"),style="text-align: center;",
                      style = "margin-bottom:-180px;"),
                  div(strong("Putri Nisrina Az-Zahra"),style="text-align: center;"),
                  div(strong("5003201158"),style="text-align: center;")
              ),
              box(width = 12,
                  div(strong("Latar Belakang"),style="text-align: center;font size: 160%")),
              box(width = 12,
                  status = NULL,
                  div(strong("Kendaraan Bermotor"),style="text-align: center;font-size: 120%"),
                  div(("Kendaraan bermotor adalah setiap kendaraan yang digerakkan oleh peralatan teknik yang ada pada kendaraan tersebut, biasanya digunakan untuk angkutan orang atau barang di atas jalan raya selain kendaraan yang berjalan di atas rel. Kendaraan bermotor yang dicatat adalah semua jenis kendaraan kecuali kendaraan bermotor TNI/Polri dan Korps Diplomatik."),
                      style="text-align: center;"),
                  br(),
                  div(strong("Kereta Api"), style="text-align: center; font-size: 120%"),
                  div(("Kereta api adalah kendaraan dengan tenaga gerak (listrik, diesel atau tenaga uap) yang berjalan sendiri maupun dirangkaikan dengan kendaraan lain, yang akan sedang bergerak di atas rel, terdiri dari kereta penumpang dan kereta barang."),
                      style="text-align: center;"),
                  br(),
                  div(strong("Kantor Pos"),style="text-align: center;font-size: 120%"),
                  div(("Kantor Pos adalah tempat pemberi pelayanan komunikasi tertulis dan atau surat elektronik, layanan paket, layanan logistik, layanan transaksi keuangan, dan layanan keagenan pos untuk kepentingan umum. Rumah pos berfungsi sama seperti kantor pos dan kantor pos pembantu, bedanya rumah pos biasanya terletak di daerah terpencil"),
                      style="text-align: center;"),
                  br(),
                  div(("Infrastruktur jalan raya merupakan salah satu yang terpenting dalam sektor transportasi untuk menunjang perekonomian suatu daerah. pada tahun 2021 Panjang jalan raya di Jawa Timur yang tergolong jalan dalam kewenangan provinsi tercatat sepanjang 1.421 km. Sementara panjang jalan di Provinsi Jawa Timur yang tergolong jalan kewenangan jalan nasional tercatat sepanjang 2.361,23 km. Tranportasi darat di Provinsi Jawa Timur yang cukup populer digunakan masyarakat adalah kereta api. Tercatat pada Desember 2021, jumlah penumpang naik melalui stasiun keberangkatan DAOP VII sebanyak 211.996 penumpang, melalui DAOP VIII sebanyak 689.424 penumpang, dan melalui DAOP IX sebanyak 118.648 penumpang. Untuk menunjang sarana komunikasi, tercatat terdapat 596 kantor pos dan kantor pos pembantu di Provinsi Jawa Timur. Kota Surabaya merupakan wilayah dengan jumlah kantor pos terbanyak, yaitu sebanyak 50 kantor pos. Untuk komunikasi digital, pada tahun 2021 masih terdapat 17 desa yang tidak ada sinyal telepon dan 111 desa masih dengan sinyal internet Edge atau GPRS."),
                      style="text-align: center;"),
                  br(),
                  div(("Dengan banyaknya data yang tersedia khususnya di bidang transportasi dan komunikasi, maka dari itu dashboard ini hadir agar pembaca mudah menggunakan data yang ada untuk keperluan pribadi/instansi (misal, analisis data). Data yang ditampilkan tidak hanya berasal dari Provinsi Jawa Timur, tetapi beserta kabupaten/kota nya pula."),
                      style="text-align: center;"))
      ),
      
      #INPUT DATA 
      tabItem(tabName = "Transportasi_3",div(h1("Data Panjang Jalan di Jawa Timur (km)"),style="text-align: center;"),
              fluidRow(
                tabBox(id = "datake1",
                       tabPanel(title = "Tingkat Kewenangan", DTOutput("datasatu") %>% withSpinner(color = "purple")),
                       tabPanel(title = "Jenis Permukaan", DTOutput("datatiga") %>% withSpinner(color = "purple")),
                       tabPanel(title = "Kondisi Jalan", DTOutput("dataempat") %>% withSpinner(color = "purple")),
                       width = 12)
              )),
      tabItem(tabName = "Transportasi_4",div(h1("Kendaraan Bermotor yang Didaftarkan Menurut Kabupaten/Kota dan Jenis Kendaraan di Provinsi Jawa Timur (unit)"),style="text-align: center;"),
              fluidRow(
                tabBox(id = "datake2",
                       tabPanel(title = "Jumlah Kendaraan Bermotor yang Didaftarkan", DTOutput("datadua") %>% withSpinner(color = "purple")), 
                       width = 12)
              )),
      tabItem(tabName = "Transportasi_5",div(h1("Jumlah Penumpang Kendaraan di Jawa Timur"),style="text-align: center;"),
              fluidRow(
                tabBox(id = "datake5",
                       tabPanel(title = "Kereta Api", DTOutput("datalima") %>% withSpinner(color = "purple")), 
                       tabPanel(title = "Pesawat Udara", DTOutput("datadelapan") %>% withSpinner(color = "purple")), 
                       width = 12)
              )),
      
      tabItem(tabName = "Komunikasi_3",div(h1("Jumlah Kantor Pos Pembantu Menurut Kabupaten/Kota di Provinsi Jawa Timur"),style="text-align: center;"),
              fluidRow(
                tabBox(id = "datake6",
                       tabPanel(title = "Jumlah Kantor Pos", DTOutput("dataenam") %>% withSpinner(color = "purple")), 
                       width = 12)
              )),
      tabItem(tabName = "Komunikasi_4",div(h1("Banyaknya Desa/Kelurahan Menurut Kabupaten/Kota dan Penerimaan Sinyal Internet Telepon Seluler"),style="text-align: center;"),
              fluidRow(
                tabBox(id = "datake7",
                       tabPanel(title = "Banyaknya Desa/Kelurahan Menurut Kabupaten/Kota dan Penerimaan Sinyal Internet", 
                                DTOutput("datatujuh") %>% withSpinner(color = "purple")), 
                       width = 12)
              )),
      
      
      #BOX
      tabItem(tabName = "Transportasi",
              fluidRow(
                box(width = 12,
                    div(strong("Transportasi - Jawa Timur"),style="text-align: center;font-size: 160%"),
                )),
              br(),
              fluidRow(
                valueBox("Penumpang : 13.359.021","98 Stasiun Kereta Api (2019)",color = "purple",icon = icon("train"), width = 6),
                valueBox("Penumpang : 6.288.591","Djuanda International Airport (2019)",color = "black",icon = icon("plane"), width = 6)
              ),
              fluidRow(
                box(title = "Pilih Tahun : ",width = 6, background = "navy", collapsible = T,
                    status = "info",solidHeader = TRUE, 
                    selectInput("year","Tahun",choices = unique(data1$`Akhir Tahun`)),
                ),
                box(title = "Pilih Jenis Kendaraan : ",width = 6, background = "navy", collapsible = T,
                    status = "info",solidHeader = TRUE, 
                    selectInput("Kendaraan","Jenis Kendaraan",choices = c("Mobil.Penumpang", "Bus", "Sepeda.Motor", "Truk")),
                )
              ),
              fluidRow(
                box(plotOutput("plot1") %>% withSpinner(color = "purple"), collapsible = T), 
                box(plotOutput("plot2") %>% withSpinner(color = "purple"), collapsible = T)
              ),
              fluidRow(
                valueBox("Kondisi Jalan","di Kabupaten/Kota Jawa Timur",color = "black",icon = icon("road"), width = 6),
                valueBox("Jumlah Penumpang Pesawat","Sepanjang tahun 2017-2021",color = "purple",icon = icon("plane"), width = 6)
              ),
              fluidRow(
                box(plotOutput("plot3") %>% withSpinner(color = "purple"),collapsible = T ),
                box(plotOutput("plot6") %>% withSpinner(color = "purple"),collapsible = T )
              ),
              fluidRow(
                box(width = 12,
                    div(strong("Jumlah Penumpang Tahun 2019"),style="text-align: center;font-size: 160%"),
                )),
              br(),
              fluidRow(width = 12, highchartOutput('line'))),
      tabItem(tabName = "Komunikasi",
              fluidRow(
                box(width = 12,
                    div(strong("Komunikasi - Jawa Timur"),style="text-align: center;font-size: 160%"),
                )),
              br(),
              fluidRow(
                valueBox("Jumlah Kantor Pos : 597","Kantor Pos Jawa Timur", color = "black",icon = icon("home"), width = 6),
                valueBox("Penerima Sinyal : 8.478","17 Desa belum mendapatkan sinyal internet", color = "purple",icon = icon("wifi"), width = 6)
              ),
              fluidRow(
                box(title = "Jenis Jaringan Sinyal Internet : ",width = 12, background = "navy", collapsible = T,
                    status = "info",solidHeader = TRUE, 
                    selectInput("sinyal","Jenis Sinyal",choices = c("FourG.LTE", "ThreeG.H.EVDO", "TwoG.E.GPRS", "Tidak.Ada")
                    ))),
              fluidRow(
                box(width = 12, plotOutput("plot7") %>% withSpinner(color = "purple"), collapsible = T)
              ),
              fluidRow(
                box(width = 12,
                    div(strong("Jumlah Penumpang Tahun 2019"),style="text-align: center;font-size: 160%"),
                )),
              br(),
              fluidRow(highchartOutput('line2')))
    ),
    tags$head(tags$style(HTML('* {font-family: "Montserrat"};')))
  ),
  skin = "purple",
  tags$head(tags$style(HTML('* {font-family: "Montserrat"};')))
)


#SERVER
server <- function(input, output) {
  #INPUT
  datasatu<-reactive((data1))
  output$datasatu <- renderDT(datatable(datasatu(), rownames = F,
                                        options = list(
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                            "}")
                                        )))
  datadua<-reactive((data2))
  output$datadua <- renderDT(datatable(datadua(), rownames = F,
                                       options = list(
                                         initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                           "}")
                                       )))
  datatiga<-reactive((data3))
  output$datatiga <- renderDT(datatable(datatiga(), rownames = F,
                                        options = list(
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                            "}")
                                        )))
  dataempat<-reactive((data4))
  output$dataempat <- renderDT(datatable(dataempat(), rownames = F,
                                         options = list(
                                           initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")
                                         )))
  datalima<-reactive((data6))
  output$datalima <- renderDT(datatable(datalima(), rownames = F,
                                        options = list(
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                            "}")
                                        )))
  dataenam<-reactive((data5))
  output$dataenam <- renderDT(datatable(dataenam(), rownames = F,
                                        options = list(
                                          initComplete = JS(
                                            "function(settings, json) {",
                                            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                            "}")
                                        )))
  datatujuh<-reactive((data7))
  output$datatujuh <- renderDT(datatable(datatujuh(), rownames = F,
                                         options = list(
                                           initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")
                                         )))
  
  datadelapan<-reactive((data82))
  output$datadelapan <- renderDT(datatable(datadelapan(), rownames = F,
                                           options = list(
                                             initComplete = JS(
                                               "function(settings, json) {",
                                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                               "}")
                                           )))
  
  data72$Tahun <- as.factor(data72$Tahun)
  data2$Tahun <- as.factor(data2$Tahun)
  
  #PETA
  bb_data <- data.frame(bb_data)
  bb_data$lat <-  as.numeric(bb_data$lat)
  bb_data$long <-  as.numeric(bb_data$long)
  
  # new column for the popup label
  
  bb_data <- mutate(bb_data, cntnt=paste0('<strong>Kota/Kabupaten : </strong>',Kabupaten.Kota,
                                          '<br><br><strong>Latitude :</strong> ',lat,
                                          '<br><br><strong>Longtitude :</strong> ',long,
                                          '<br><br><strong>Panjang Jalan :</strong> ',Panjang.Jalan,
                                          '<br><br><strong>Jumlah Mobil yang Didaftarkan :</strong> ',Mobil.Penumpang,
                                          '<br><br><strong>Jumlah Bus yang Didaftarkan :</strong> ',Bus,
                                          '<br><br><strong>Jumlah Truk yang Didaftarkan :</strong> ',Truk,
                                          '<br><br><strong>Jumlah Motor yang Didaftarkan :</strong> ',Sepeda.Motor,
                                          '<br><br><strong>Jumlah Kantor Pos :</strong> ',Jumlah.Kantor.Pos))
  
  
  # create a color paletter for category type in the data file
  
  pal <- colorFactor(pal = c("#FF0000", 
                             "#FF0022", 
                             "#FF004D", 
                             "#FF0089",
                             "#FF00CD", 
                             "#FF00FF", 
                             "#DE00FF", 
                             "#BC00FF",
                             "#8900FF", 
                             "#5E00FF", 
                             "#2B00FF", 
                             "#006FFF",
                             "#0091FF", 
                             "#00B3FF", 
                             "#00DEFF", 
                             "#00FFC4"), 
                     domain = c("Bangkalan", 
                                "Banyuwangi", 
                                "Blitar",
                                "Bojonegoro",
                                "Bondowoso",
                                "Gresik",
                                "Jember",
                                "Jombang",
                                "Kediri",
                                "Kota Batu",
                                "Kota Blitar",
                                "Kota Kediri",
                                "Kota Madiun",
                                "Kota Malang",
                                "Kota Mojokerto",
                                "Kota Pasuruan",
                                "Kota Probolinggo",
                                "Kota Surabaya",
                                "Lamongan",
                                "Lumajang",
                                "Madiun",
                                "Magetan",
                                "Malang",
                                "Mojokerto",
                                "Nganjuk",
                                "Ngawi",
                                "Pacitan", 
                                "Pamekasan",
                                "Pasuruan",
                                "Ponorogo",
                                "Probolinggo",
                                "Sampang",
                                "Sidoarjo",
                                "Situbondo",
                                "Sumenep",
                                "Trenggalek", "Tuban"))
  
  library(dplyr)
  # OUTPUT  
  PJ1<-reactive({
    data1%>%
      filter(`Akhir Tahun`==input$year)%>%
      ggplot(aes(x=reorder(Kabupaten.Kota, Jumlah), y=Jumlah)) + xlab("Kabupaten/Kota") +
      ggtitle("Panjang Jalan Kabupaten/Kota", input$year) +geom_bar(stat="identity", width=0.6, color = "#4D4C7D") + 
      coord_flip() + theme(
        plot.title = element_text(color = "#9A86A4", size = 20, face = "bold.italic"),
        axis.title.x = element_text(color="#2E0249", size=10),
        axis.title.y = element_text(color="#2E0249", size=10),
        axis.text = element_text(size=5, color = "black"),
        panel.background = element_blank()
      )
  })
  
  kend1 <- reactive({
    data2%>%
      ggplot(aes_string(x='Tahun', y=input$Kendaraan)) + geom_violin(color = "#2E0249", fill = "#9A86A4") +
      ggtitle("Jumlah Kendaraan Terdaftar")+  theme(
        plot.title = element_text(color = "#9A86A4", size = 20, face = "bold.italic"),
        axis.title.x = element_text(color="#2E0249", size=12),
        axis.title.y = element_text(color="#2E0249", size=12, face = "bold"),
        panel.background = element_blank()
      )
  })
  
  pesawat1 <- reactive({
    data8%>%
      ggplot(aes(x=month, y=input$tahun, group = 1)) + geom_line(color = "#5B7DB1", size = 1.3) + geom_point(color = "#5B7DB1", size = 3) +
      ggtitle("Jumlah Penumpang Pesawat Udara") + labs(y="Jumlah Penumpang ") + labs(x="Bulan") + theme(
        plot.title = element_text(color = "#5B7DB1", size = 22, face = "bold.italic"), panel.background = element_rect(fill = "#F7F5F2")
      ) + scale_x_continuous(limits = c(0, 12), breaks = c(01,02,03,04,05,06,07,08,09,10,11,12))
  })
  
  plotsinyal <- reactive({
    data72%>%
      ggplot(aes_string(x='Tahun', y=input$sinyal)) + geom_bar(stat = "identity") +
      scale_fill_brewer(palette="Greys") + ggtitle("4G/LTE - 3G/H/H+/EVDO - 2,5G/E/GPRS - Tidak Ada") + theme(
        plot.title = element_text(color = "#413F42", size = 24, face = "bold"),
        panel.background = element_blank())
  })
  
  output$plot1<-renderPlot({
    PJ1()
  }) 
  output$plot2<-renderPlot({
    kend1()
  }) 
  output$plot3<-renderPlot({
    jalan
  }) 
  output$plot4<-renderPlot({
    pesawat1()
  }) 
  output$plot5<-renderPlot({
    a
  }) 
  output$plot6<-renderPlot({
    b
  }) 
  output$plot7<-renderPlot({
    plotsinyal()
  }) 
  
  output$line <-renderHighchart({
    highchart() %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_chart(type = 'line') %>%
      hc_series( list(name = 'Kereta', data =naikkereta$penumpang[naikkereta$month], color = 'red', marker = list(symbol = 'circle') ),
                 list(name = 'Pesawat', data =data8$`2019`[data8$Bulan], color = 'green', marker = list(symbol = 'triangle')  )
      )%>%
      hc_xAxis( categories = unique(naikkereta$month) ) %>%
      hc_yAxis( title = list(text = "Jumlah Penumpang"))%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F), 
        #stacking = "normal", 
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} penumpang"),
                 headerFormat = '<span style="font-size: 13px">Bulan ke - {point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
  })
  
  output$line2 <-renderHighchart({
    highchart() %>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
      hc_chart(type = 'line') %>%
      hc_series(list(name = 'Jumlah Akses', data =data9$Total[data9$No.], color = 'navy', marker = list(symbol = 'circle')  )
      )%>%
      hc_xAxis( categories = unique(data9$No.) ) %>%
      hc_yAxis( title = list(text = "Jumlah Penumpang"))%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F), 
        #stacking = "normal", 
        enableMouseTracking = T ) 
      )%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y} "),
                 headerFormat = '<span style="font-size: 13px">{point.key}</span>'
      ) %>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
  })
  
  output$video <- renderUI({
    id <- videos |>
      filter(welcome == input$welcome) |>
      pull(id) |>
      first()
    
    paste0(
      "<iframe width='1000' height='530' src='https://www.youtube.com/embed/",
      id,
      "' title='YouTube video player'",
      "frameborder='0' allow='accelerometer; autoplay; clipboard-write;",
      "encrypted-media;gyroscope; picture-in-picture' allowfullscreen></iframe>"
    ) |> HTML()
  })
  
  
  output$farsya<-renderImage({
    list(src="farsya.jpeg",height = 220, width = 170)
  },deleteFile = F)
  output$zahra<-renderImage({
    list(src="zahra.jpeg",height = 220, width = 170)
  },deleteFile = F)
  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data) %>% 
      addProviderTiles(providers$Stamen.Watercolor, group = "Stamen Watercolor", options = providerTileOptions(noWrap = TRUE)) %>%#, minZoom = 4)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012, group = "Nasa Earth at Night", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen Terrain Background", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery", options = providerTileOptions(noWrap = TRUE)) %>%
      addCircles(lng = ~long, lat = ~lat) %>% 
      addMarkers(
        ~long,
        ~lat,
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(pal=pal, values=bb_data$Kabupaten.Kota,opacity=1, na.label = "Not Available")%>%
      addLayersControl(
        baseGroups = c("Stamen Watercolor","Open Street Map","Nasa Earth at Night","Stamen Terrain Background","Esri World Imagery"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addCircleMarkers(data = bb_data, lat =  ~lat, lng =~long, 
                       radius = 10, popup = ~as.character(cntnt), 
                       color = ~pal(Kabupaten.Kota),
                       stroke = FALSE, fillOpacity = 0.8)%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
}


shinyApp(ui, server)

