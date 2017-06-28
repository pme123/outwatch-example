package nextds.client.components

import outwatch.dom._

/**
  * Provides type-safe access to Font Awesome icons
  */
object Icon {
  val dragHandle = "glyphicon-move"
  type Icon = VNode

  def apply(name: String): Icon = i(className := s"fa fa-$name $dragHandle")

  def loadingIcon: Icon = i(className := s"fa fa-spinner fa-spin loading-spinner")

  def adjust: Icon = apply("adjust")

  def adn: Icon = apply("adn")

  def alignCenter: Icon = apply("align-center")

  def alignJustify: Icon = apply("align-justify")

  def alignLeft: Icon = apply("align-left")

  def alignRight: Icon = apply("align-right")

  def ambulance: Icon = apply("ambulance")

  def anchor: Icon = apply("anchor")

  def android: Icon = apply("android")

  def angellist: Icon = apply("angellist")

  def angleDoubleDown: Icon = apply("angle-double-down")

  def angleDoubleLeft: Icon = apply("angle-double-left")

  def angleDoubleRight: Icon = apply("angle-double-right")

  def angleDoubleUp: Icon = apply("angle-double-up")

  def angleDown: Icon = apply("angle-down")

  def angleLeft: Icon = apply("angle-left")

  def angleRight: Icon = apply("angle-right")

  def angleUp: Icon = apply("angle-up")

  def apple: Icon = apply("apple")

  def archive: Icon = apply("archive")

  def areaChart: Icon = apply("area-chart")

  def arrowCircleDown: Icon = apply("arrow-circle-down")

  def arrowCircleLeft: Icon = apply("arrow-circle-left")

  def arrowCircleODown: Icon = apply("arrow-circle-o-down")

  def arrowCircleOLeft: Icon = apply("arrow-circle-o-left")

  def arrowCircleORight: Icon = apply("arrow-circle-o-right")

  def arrowCircleOUp: Icon = apply("arrow-circle-o-up")

  def arrowCircleRight: Icon = apply("arrow-circle-right")

  def arrowCircleUp: Icon = apply("arrow-circle-up")

  def arrowDown: Icon = apply("arrow-down")

  def arrowLeft: Icon = apply("arrow-left")

  def arrowRight: Icon = apply("arrow-right")

  def arrowUp: Icon = apply("arrow-up")

  def arrows: Icon = apply("arrows")

  def arrowsAlt: Icon = apply("arrows-alt")

  def arrowsH: Icon = apply("arrows-h")

  def arrowsV: Icon = apply("arrows-v")

  def asterisk: Icon = apply("asterisk")

  def at: Icon = apply("at")

  def automobile: Icon = apply("automobile")

  def backward: Icon = apply("backward")

  def ban: Icon = apply("ban")

  def bank: Icon = apply("bank")

  def barChart: Icon = apply("bar-chart")

  def barChartO: Icon = apply("bar-chart-o")

  def barcode: Icon = apply("barcode")

  def bars: Icon = apply("bars")

  def bed: Icon = apply("bed")

  def beer: Icon = apply("beer")

  def behance: Icon = apply("behance")

  def behanceSquare: Icon = apply("behance-square")

  def bell: Icon = apply("bell")

  def bellO: Icon = apply("bell-o")

  def bellSlash: Icon = apply("bell-slash")

  def bellSlashO: Icon = apply("bell-slash-o")

  def bicycle: Icon = apply("bicycle")

  def binoculars: Icon = apply("binoculars")

  def birthdayCake: Icon = apply("birthday-cake")

  def bitbucket: Icon = apply("bitbucket")

  def bitbucketSquare: Icon = apply("bitbucket-square")

  def bitcoin: Icon = apply("bitcoin")

  def bold: Icon = apply("bold")

  def bolt: Icon = apply("bolt")

  def bomb: Icon = apply("bomb")

  def book: Icon = apply("book")

  def bookmark: Icon = apply("bookmark")

  def bookmarkO: Icon = apply("bookmark-o")

  def briefcase: Icon = apply("briefcase")

  def btc: Icon = apply("btc")

  def bug: Icon = apply("bug")

  def building: Icon = apply("building")

  def buildingO: Icon = apply("building-o")

  def bullhorn: Icon = apply("bullhorn")

  def bullseye: Icon = apply("bullseye")

  def bus: Icon = apply("bus")

  def buysellads: Icon = apply("buysellads")

  def cab: Icon = apply("cab")

  def calculator: Icon = apply("calculator")

  def calendar: Icon = apply("calendar")

  def calendarO: Icon = apply("calendar-o")

  def camera: Icon = apply("camera")

  def cameraRetro: Icon = apply("camera-retro")

  def car: Icon = apply("car")

  def caretDown: Icon = apply("caret-down")

  def caretLeft: Icon = apply("caret-left")

  def caretRight: Icon = apply("caret-right")

  def caretSquareODown: Icon = apply("caret-square-o-down")

  def caretSquareOLeft: Icon = apply("caret-square-o-left")

  def caretSquareORight: Icon = apply("caret-square-o-right")

  def caretSquareOUp: Icon = apply("caret-square-o-up")

  def caretUp: Icon = apply("caret-up")

  def cartArrowDown: Icon = apply("cart-arrow-down")

  def cartPlus: Icon = apply("cart-plus")

  def cc: Icon = apply("cc")

  def ccAmex: Icon = apply("cc-amex")

  def ccDiscover: Icon = apply("cc-discover")

  def ccMastercard: Icon = apply("cc-mastercard")

  def ccPaypal: Icon = apply("cc-paypal")

  def ccStripe: Icon = apply("cc-stripe")

  def ccVisa: Icon = apply("cc-visa")

  def certificate: Icon = apply("certificate")

  def chain: Icon = apply("chain")

  def chainBroken: Icon = apply("chain-broken")

  def check: Icon = apply("check")

  def checkCircle: Icon = apply("check-circle")

  def checkCircleO: Icon = apply("check-circle-o")

  def checkSquare: Icon = apply("check-square")

  def checkSquareO: Icon = apply("check-square-o")

  def chevronCircleDown: Icon = apply("chevron-circle-down")

  def chevronCircleLeft: Icon = apply("chevron-circle-left")

  def chevronCircleRight: Icon = apply("chevron-circle-right")

  def chevronCircleUp: Icon = apply("chevron-circle-up")

  def chevronDown: Icon = apply("chevron-down")

  def chevronLeft: Icon = apply("chevron-left")

  def chevronRight: Icon = apply("chevron-right")

  def chevronUp: Icon = apply("chevron-up")

  def child: Icon = apply("child")

  def circle: Icon = apply("circle")

  def circleO: Icon = apply("circle-o")

  def circleONotch: Icon = apply("circle-o-notch")

  def circleThin: Icon = apply("circle-thin")

  def clipboard: Icon = apply("clipboard")

  def clockO: Icon = apply("clock-o")

  def close: Icon = apply("close")

  def cloud: Icon = apply("cloud")

  def cloudDownload: Icon = apply("cloud-download")

  def cloudUpload: Icon = apply("cloud-upload")

  def cny: Icon = apply("cny")

  def code: Icon = apply("code")

  def codeFork: Icon = apply("code-fork")

  def codepen: Icon = apply("codepen")

  def coffee: Icon = apply("coffee")

  def cog: Icon = apply("cog")

  def cogs: Icon = apply("cogs")

  def columns: Icon = apply("columns")

  def comment: Icon = apply("comment")

  def commentO: Icon = apply("comment-o")

  def comments: Icon = apply("comments")

  def commentsO: Icon = apply("comments-o")

  def compass: Icon = apply("compass")

  def compress: Icon = apply("compress")

  def connectdevelop: Icon = apply("connectdevelop")

  def copy: Icon = apply("copy")

  def copyright: Icon = apply("copyright")

  def creditCard: Icon = apply("credit-card")

  def crop: Icon = apply("crop")

  def crosshairs: Icon = apply("crosshairs")

  def css3: Icon = apply("css3")

  def cube: Icon = apply("cube")

  def cubes: Icon = apply("cubes")

  def cut: Icon = apply("cut")

  def cutlery: Icon = apply("cutlery")

  def dashboard: Icon = apply("dashboard")

  def dashcube: Icon = apply("dashcube")

  def database: Icon = apply("database")

  def dedent: Icon = apply("dedent")

  def delicious: Icon = apply("delicious")

  def desktop: Icon = apply("desktop")

  def deviantart: Icon = apply("deviantart")

  def diamond: Icon = apply("diamond")

  def digg: Icon = apply("digg")

  def dollar: Icon = apply("dollar")

  def dotCircleO: Icon = apply("dot-circle-o")

  def download: Icon = apply("download")

  def dribbble: Icon = apply("dribbble")

  def dropbox: Icon = apply("dropbox")

  def drupal: Icon = apply("drupal")

  def edit: Icon = apply("edit")

  def eject: Icon = apply("eject")

  def ellipsisH: Icon = apply("ellipsis-h")

  def ellipsisV: Icon = apply("ellipsis-v")

  def empire: Icon = apply("empire")

  def envelope: Icon = apply("envelope")

  def envelopeO: Icon = apply("envelope-o")

  def envelopeSquare: Icon = apply("envelope-square")

  def eraser: Icon = apply("eraser")

  def eur: Icon = apply("eur")

  def euro: Icon = apply("euro")

  def exchange: Icon = apply("exchange")

  def exclamation: Icon = apply("exclamation")

  def exclamationCircle: Icon = apply("exclamation-circle")

  def exclamationTriangle: Icon = apply("exclamation-triangle")

  def expand: Icon = apply("expand")

  def externalLink: Icon = apply("external-link")

  def externalLinkSquare: Icon = apply("external-link-square")

  def eye: Icon = apply("eye")

  def eyeSlash: Icon = apply("eye-slash")

  def eyedropper: Icon = apply("eyedropper")

  def facebook: Icon = apply("facebook")

  def facebookF: Icon = apply("facebook-f")

  def facebookOfficial: Icon = apply("facebook-official")

  def facebookSquare: Icon = apply("facebook-square")

  def fastBackward: Icon = apply("fast-backward")

  def fastForward: Icon = apply("fast-forward")

  def fax: Icon = apply("fax")

  def female: Icon = apply("female")

  def fighterJet: Icon = apply("fighter-jet")

  def file: Icon = apply("file")

  def fileArchiveO: Icon = apply("file-archive-o")

  def fileAudioO: Icon = apply("file-audio-o")

  def fileCodeO: Icon = apply("file-code-o")

  def fileExcelO: Icon = apply("file-excel-o")

  def fileImageO: Icon = apply("file-image-o")

  def fileMovieO: Icon = apply("file-movie-o")

  def fileO: Icon = apply("file-o")

  def filePdfO: Icon = apply("file-pdf-o")

  def filePhotoO: Icon = apply("file-photo-o")

  def filePictureO: Icon = apply("file-picture-o")

  def filePowerpointO: Icon = apply("file-powerpoint-o")

  def fileSoundO: Icon = apply("file-sound-o")

  def fileText: Icon = apply("file-text")

  def fileTextO: Icon = apply("file-text-o")

  def fileVideoO: Icon = apply("file-video-o")

  def fileWordO: Icon = apply("file-word-o")

  def fileZipO: Icon = apply("file-zip-o")

  def filesO: Icon = apply("files-o")

  def film: Icon = apply("film")

  def filter: Icon = apply("filter")

  def fire: Icon = apply("fire")

  def fireExtinguisher: Icon = apply("fire-extinguisher")

  def flag: Icon = apply("flag")

  def flagCheckered: Icon = apply("flag-checkered")

  def flagO: Icon = apply("flag-o")

  def flash: Icon = apply("flash")

  def flask: Icon = apply("flask")

  def flickr: Icon = apply("flickr")

  def floppyO: Icon = apply("floppy-o")

  def folder: Icon = apply("folder")

  def folderO: Icon = apply("folder-o")

  def folderOpen: Icon = apply("folder-open")

  def folderOpenO: Icon = apply("folder-open-o")

  def font: Icon = apply("font")

  def forumbee: Icon = apply("forumbee")

  def forward: Icon = apply("forward")

  def foursquare: Icon = apply("foursquare")

  def frownO: Icon = apply("frown-o")

  def futbolO: Icon = apply("futbol-o")

  def gamepad: Icon = apply("gamepad")

  def gavel: Icon = apply("gavel")

  def gbp: Icon = apply("gbp")

  def ge: Icon = apply("ge")

  def gear: Icon = apply("gear")

  def gears: Icon = apply("gears")

  def genderless: Icon = apply("genderless")

  def gift: Icon = apply("gift")

  def git: Icon = apply("git")

  def gitSquare: Icon = apply("git-square")

  def github: Icon = apply("github")

  def githubAlt: Icon = apply("github-alt")

  def githubSquare: Icon = apply("github-square")

  def gittip: Icon = apply("gittip")

  def glass: Icon = apply("glass")

  def globe: Icon = apply("globe")

  def google: Icon = apply("google")

  def googlePlus: Icon = apply("google-plus")

  def googlePlusSquare: Icon = apply("google-plus-square")

  def googleWallet: Icon = apply("google-wallet")

  def graduationCap: Icon = apply("graduation-cap")

  def gratipay: Icon = apply("gratipay")

  def group: Icon = apply("group")

  def hSquare: Icon = apply("h-square")

  def hackerNews: Icon = apply("hacker-news")

  def handODown: Icon = apply("hand-o-down")

  def handOLeft: Icon = apply("hand-o-left")

  def handORight: Icon = apply("hand-o-right")

  def handOUp: Icon = apply("hand-o-up")

  def hddO: Icon = apply("hdd-o")

  def header: Icon = apply("header")

  def headphones: Icon = apply("headphones")

  def heart: Icon = apply("heart")

  def heartO: Icon = apply("heart-o")

  def heartbeat: Icon = apply("heartbeat")

  def history: Icon = apply("history")

  def home: Icon = apply("home")

  def hospitalO: Icon = apply("hospital-o")

  def hotel: Icon = apply("hotel")

  def html5: Icon = apply("html5")

  def ils: Icon = apply("ils")

  def image: Icon = apply("image")

  def inbox: Icon = apply("inbox")

  def indent: Icon = apply("indent")

  def info: Icon = apply("info")

  def infoCircle: Icon = apply("info-circle")

  def inr: Icon = apply("inr")

  def instagram: Icon = apply("instagram")

  def institution: Icon = apply("institution")

  def ioxhost: Icon = apply("ioxhost")

  def italic: Icon = apply("italic")

  def joomla: Icon = apply("joomla")

  def jpy: Icon = apply("jpy")

  def jsfiddle: Icon = apply("jsfiddle")

  def key: Icon = apply("key")

  def keyboardO: Icon = apply("keyboard-o")

  def krw: Icon = apply("krw")

  def language: Icon = apply("language")

  def laptop: Icon = apply("laptop")

  def lastfm: Icon = apply("lastfm")

  def lastfmSquare: Icon = apply("lastfm-square")

  def leaf: Icon = apply("leaf")

  def leanpub: Icon = apply("leanpub")

  def legal: Icon = apply("legal")

  def lemonO: Icon = apply("lemon-o")

  def levelDown: Icon = apply("level-down")

  def levelUp: Icon = apply("level-up")

  def lifeBouy: Icon = apply("life-bouy")

  def lifeBuoy: Icon = apply("life-buoy")

  def lifeRing: Icon = apply("life-ring")

  def lifeSaver: Icon = apply("life-saver")

  def lightbulbO: Icon = apply("lightbulb-o")

  def lineChart: Icon = apply("line-chart")

  def link: Icon = apply("link")

  def linkedin: Icon = apply("linkedin")

  def linkedinSquare: Icon = apply("linkedin-square")

  def linux: Icon = apply("linux")

  def list: Icon = apply("list")

  def listAlt: Icon = apply("list-alt")

  def listOl: Icon = apply("list-ol")

  def listUl: Icon = apply("list-ul")

  def locationArrow: Icon = apply("location-arrow")

  def lock: Icon = apply("lock")

  def longArrowDown: Icon = apply("long-arrow-down")

  def longArrowLeft: Icon = apply("long-arrow-left")

  def longArrowRight: Icon = apply("long-arrow-right")

  def longArrowUp: Icon = apply("long-arrow-up")

  def magic: Icon = apply("magic")

  def magnet: Icon = apply("magnet")

  def mailForward: Icon = apply("mail-forward")

  def mailReply: Icon = apply("mail-reply")

  def mailReplyAll: Icon = apply("mail-reply-all")

  def male: Icon = apply("male")

  def mapMarker: Icon = apply("map-marker")

  def mars: Icon = apply("mars")

  def marsDouble: Icon = apply("mars-double")

  def marsStroke: Icon = apply("mars-stroke")

  def marsStrokeH: Icon = apply("mars-stroke-h")

  def marsStrokeV: Icon = apply("mars-stroke-v")

  def maxcdn: Icon = apply("maxcdn")

  def meanpath: Icon = apply("meanpath")

  def medium: Icon = apply("medium")

  def medkit: Icon = apply("medkit")

  def mehO: Icon = apply("meh-o")

  def mercury: Icon = apply("mercury")

  def microphone: Icon = apply("microphone")

  def microphoneSlash: Icon = apply("microphone-slash")

  def minus: Icon = apply("minus")

  def minusCircle: Icon = apply("minus-circle")

  def minusSquare: Icon = apply("minus-square")

  def minusSquareO: Icon = apply("minus-square-o")

  def mobile: Icon = apply("mobile")

  def mobilePhone: Icon = apply("mobile-phone")

  def money: Icon = apply("money")

  def moonO: Icon = apply("moon-o")

  def mortarBoard: Icon = apply("mortar-board")

  def motorcycle: Icon = apply("motorcycle")

  def music: Icon = apply("music")

  def navicon: Icon = apply("navicon")

  def neuter: Icon = apply("neuter")

  def newspaperO: Icon = apply("newspaper-o")

  def openid: Icon = apply("openid")

  def outdent: Icon = apply("outdent")

  def pagelines: Icon = apply("pagelines")

  def paintBrush: Icon = apply("paint-brush")

  def paperPlane: Icon = apply("paper-plane")

  def paperPlaneO: Icon = apply("paper-plane-o")

  def paperclip: Icon = apply("paperclip")

  def paragraph: Icon = apply("paragraph")

  def paste: Icon = apply("paste")

  def pause: Icon = apply("pause")

  def paw: Icon = apply("paw")

  def paypal: Icon = apply("paypal")

  def pencil: Icon = apply("pencil")

  def pencilSquare: Icon = apply("pencil-square")

  def pencilSquareO: Icon = apply("pencil-square-o")

  def phone: Icon = apply("phone")

  def phoneSquare: Icon = apply("phone-square")

  def photo: Icon = apply("photo")

  def pictureO: Icon = apply("picture-o")

  def pieChart: Icon = apply("pie-chart")

  def piedPiper: Icon = apply("pied-piper")

  def piedPiperAlt: Icon = apply("pied-piper-alt")

  def pinterest: Icon = apply("pinterest")

  def pinterestP: Icon = apply("pinterest-p")

  def pinterestSquare: Icon = apply("pinterest-square")

  def plane: Icon = apply("plane")

  def play: Icon = apply("play")

  def playCircle: Icon = apply("play-circle")

  def playCircleO: Icon = apply("play-circle-o")

  def plug: Icon = apply("plug")

  def plus: Icon = apply("plus")

  def plusCircle: Icon = apply("plus-circle")

  def plusSquare: Icon = apply("plus-square")

  def plusSquareO: Icon = apply("plus-square-o")

  def powerOff: Icon = apply("power-off")

  def print: Icon = apply("print")

  def puzzlePiece: Icon = apply("puzzle-piece")

  def qq: Icon = apply("qq")

  def qrcode: Icon = apply("qrcode")

  def question: Icon = apply("question")

  def questionCircle: Icon = apply("question-circle")

  def quoteLeft: Icon = apply("quote-left")

  def quoteRight: Icon = apply("quote-right")

  def ra: Icon = apply("ra")

  def random: Icon = apply("random")

  def rebel: Icon = apply("rebel")

  def recycle: Icon = apply("recycle")

  def reddit: Icon = apply("reddit")

  def redditSquare: Icon = apply("reddit-square")

  def refresh: Icon = apply("refresh")

  def remove: Icon = apply("remove")

  def renren: Icon = apply("renren")

  def reorder: Icon = apply("reorder")

  def repeat: Icon = apply("repeat")

  def reply: Icon = apply("reply")

  def replyAll: Icon = apply("reply-all")

  def retweet: Icon = apply("retweet")

  def rmb: Icon = apply("rmb")

  def road: Icon = apply("road")

  def rocket: Icon = apply("rocket")

  def rotateLeft: Icon = apply("rotate-left")

  def rotateRight: Icon = apply("rotate-right")

  def rouble: Icon = apply("rouble")

  def rss: Icon = apply("rss")

  def rssSquare: Icon = apply("rss-square")

  def rub: Icon = apply("rub")

  def ruble: Icon = apply("ruble")

  def rupee: Icon = apply("rupee")

  def save: Icon = apply("save")

  def scissors: Icon = apply("scissors")

  def search: Icon = apply("search")

  def searchMinus: Icon = apply("search-minus")

  def searchPlus: Icon = apply("search-plus")

  def sellsy: Icon = apply("sellsy")

  def send: Icon = apply("send")

  def sendO: Icon = apply("send-o")

  def server: Icon = apply("server")

  def share: Icon = apply("share")

  def shareAlt: Icon = apply("share-alt")

  def shareAltSquare: Icon = apply("share-alt-square")

  def shareSquare: Icon = apply("share-square")

  def shareSquareO: Icon = apply("share-square-o")

  def shekel: Icon = apply("shekel")

  def sheqel: Icon = apply("sheqel")

  def shield: Icon = apply("shield")

  def ship: Icon = apply("ship")

  def shirtsinbulk: Icon = apply("shirtsinbulk")

  def shoppingCart: Icon = apply("shopping-cart")

  def signIn: Icon = apply("sign-in")

  def signOut: Icon = apply("sign-out")

  def signal: Icon = apply("signal")

  def simplybuilt: Icon = apply("simplybuilt")

  def sitemap: Icon = apply("sitemap")

  def skyatlas: Icon = apply("skyatlas")

  def skype: Icon = apply("skype")

  def slack: Icon = apply("slack")

  def sliders: Icon = apply("sliders")

  def slideshare: Icon = apply("slideshare")

  def smileO: Icon = apply("smile-o")

  def soccerBallO: Icon = apply("soccer-ball-o")

  def sort: Icon = apply("sort")

  def sortAlphaAsc: Icon = apply("sort-alpha-asc")

  def sortAlphaDesc: Icon = apply("sort-alpha-desc")

  def sortAmountAsc: Icon = apply("sort-amount-asc")

  def sortAmountDesc: Icon = apply("sort-amount-desc")

  def sortAsc: Icon = apply("sort-asc")

  def sortDesc: Icon = apply("sort-desc")

  def sortDown: Icon = apply("sort-down")

  def sortNumericAsc: Icon = apply("sort-numeric-asc")

  def sortNumericDesc: Icon = apply("sort-numeric-desc")

  def sortUp: Icon = apply("sort-up")

  def soundcloud: Icon = apply("soundcloud")

  def spaceShuttle: Icon = apply("space-shuttle")

  def spinner: Icon = apply("spinner")

  def spoon: Icon = apply("spoon")

  def spotify: Icon = apply("spotify")

  def square: Icon = apply("square")

  def squareO: Icon = apply("square-o")

  def stackExchange: Icon = apply("stack-exchange")

  def stackOverflow: Icon = apply("stack-overflow")

  def star: Icon = apply("star")

  def starHalf: Icon = apply("star-half")

  def starHalfEmpty: Icon = apply("star-half-empty")

  def starHalfFull: Icon = apply("star-half-full")

  def starHalfO: Icon = apply("star-half-o")

  def starO: Icon = apply("star-o")

  def steam: Icon = apply("steam")

  def steamSquare: Icon = apply("steam-square")

  def stepBackward: Icon = apply("step-backward")

  def stepForward: Icon = apply("step-forward")

  def stethoscope: Icon = apply("stethoscope")

  def stop: Icon = apply("stop")

  def streetView: Icon = apply("street-view")

  def strikethrough: Icon = apply("strikethrough")

  def stumbleupon: Icon = apply("stumbleupon")

  def stumbleuponCircle: Icon = apply("stumbleupon-circle")

  def subscript: Icon = apply("subscript")

  def subway: Icon = apply("subway")

  def suitcase: Icon = apply("suitcase")

  def sunO: Icon = apply("sun-o")

  def superscript: Icon = apply("superscript")

  def support: Icon = apply("support")

  def table: Icon = apply("table")

  def tablet: Icon = apply("tablet")

  def tachometer: Icon = apply("tachometer")

  def tag: Icon = apply("tag")

  def tags: Icon = apply("tags")

  def tasks: Icon = apply("tasks")

  def taxi: Icon = apply("taxi")

  def tencentWeibo: Icon = apply("tencent-weibo")

  def terminal: Icon = apply("terminal")

  def textHeight: Icon = apply("text-height")

  def textWidth: Icon = apply("text-width")

  def th: Icon = apply("th")

  def thLarge: Icon = apply("th-large")

  def thList: Icon = apply("th-list")

  def thumbTack: Icon = apply("thumb-tack")

  def thumbsDown: Icon = apply("thumbs-down")

  def thumbsODown: Icon = apply("thumbs-o-down")

  def thumbsOUp: Icon = apply("thumbs-o-up")

  def thumbsUp: Icon = apply("thumbs-up")

  def ticket: Icon = apply("ticket")

  def times: Icon = apply("times")

  def timesCircle: Icon = apply("times-circle")

  def timesCircleO: Icon = apply("times-circle-o")

  def tint: Icon = apply("tint")

  def toggleDown: Icon = apply("toggle-down")

  def toggleLeft: Icon = apply("toggle-left")

  def toggleOff: Icon = apply("toggle-off")

  def toggleOn: Icon = apply("toggle-on")

  def toggleRight: Icon = apply("toggle-right")

  def toggleUp: Icon = apply("toggle-up")

  def train: Icon = apply("train")

  def transgender: Icon = apply("transgender")

  def transgenderAlt: Icon = apply("transgender-alt")

  def trash: Icon = apply("trash")

  def trashO: Icon = apply("trash-o")

  def tree: Icon = apply("tree")

  def trello: Icon = apply("trello")

  def trophy: Icon = apply("trophy")

  def truck: Icon = apply("truck")

  def `try`: Icon = apply("try")

  def tty: Icon = apply("tty")

  def tumblr: Icon = apply("tumblr")

  def tumblrSquare: Icon = apply("tumblr-square")

  def turkishLira: Icon = apply("turkish-lira")

  def twitch: Icon = apply("twitch")

  def twitter: Icon = apply("twitter")

  def twitterSquare: Icon = apply("twitter-square")

  def umbrella: Icon = apply("umbrella")

  def underline: Icon = apply("underline")

  def undo: Icon = apply("undo")

  def university: Icon = apply("university")

  def unlink: Icon = apply("unlink")

  def unlock: Icon = apply("unlock")

  def unlockAlt: Icon = apply("unlock-alt")

  def unsorted: Icon = apply("unsorted")

  def upload: Icon = apply("upload")

  def usd: Icon = apply("usd")

  def user: Icon = apply("user")

  def userMd: Icon = apply("user-md")

  def userPlus: Icon = apply("user-plus")

  def userSecret: Icon = apply("user-secret")

  def userTimes: Icon = apply("user-times")

  def users: Icon = apply("users")

  def venus: Icon = apply("venus")

  def venusDouble: Icon = apply("venus-double")

  def venusMars: Icon = apply("venus-mars")

  def viacoin: Icon = apply("viacoin")

  def videoCamera: Icon = apply("video-camera")

  def vimeoSquare: Icon = apply("vimeo-square")

  def vine: Icon = apply("vine")

  def vk: Icon = apply("vk")

  def volumeDown: Icon = apply("volume-down")

  def volumeOff: Icon = apply("volume-off")

  def volumeUp: Icon = apply("volume-up")

  def warning: Icon = apply("warning")

  def wechat: Icon = apply("wechat")

  def weibo: Icon = apply("weibo")

  def weixin: Icon = apply("weixin")

  def whatsapp: Icon = apply("whatsapp")

  def wheelchair: Icon = apply("wheelchair")

  def wifi: Icon = apply("wifi")

  def windows: Icon = apply("windows")

  def won: Icon = apply("won")

  def wordpress: Icon = apply("wordpress")

  def wrench: Icon = apply("wrench")

  def xing: Icon = apply("xing")

  def xingSquare: Icon = apply("xing-square")

  def yahoo: Icon = apply("yahoo")

  def yelp: Icon = apply("yelp")

  def yen: Icon = apply("yen")

  def youtube: Icon = apply("youtube")

  def youtubePlay: Icon = apply("youtube-play")

  def youtubeSquare: Icon = apply("youtube-square")
}

