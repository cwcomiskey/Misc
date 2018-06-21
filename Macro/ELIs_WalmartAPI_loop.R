# ELIs: walmart API loop

# walmartAPI =======
key = "m9vevaewz6a6fawujxs2amnx"
categories <- walmartAPI::taxonomy(key = key)

# e.g.
orange <- walmartAPI::searching(
  query = "orange", key = key, categoryId = "976759", 
  numItems = 5, sort = "relevance") %>%
  select(itemId, name, salePrice, categoryPath,
         productUrl, offerType)

# e.g.
bread <- walmartAPI::searching(
  query = "bread", key = key, categoryId = "976759", 
  numItems = 10, sort = "relevance")

grab <- function(item, catId = NULL){
  t <- walmartAPI::searching(
    query = item, 
    key = key, 
    categoryId = catId, 
    numItems = 5, 
    sort = "relevance") %>%
    select(itemId, 
           name, 
           salePrice, 
           categoryPath,
           productUrl, 
           offerType) %>%
    .[1:5,]
  return(t)
}

t <- grab("shirt", "5438")

# Automate? =====
Appendix5 <- read.csv("~/Desktop/ODG/Macro-models/Appendix5.txt", sep="") %>%
  mutate(Category = as.character(Category), Code = as.character(Code)) %>%
  filter(nchar(Code) == 5,
         !str_detect(Category, "AND RELATED PRODUCTS"),
         !str_detect(Category, "OTHER"),
         !str_detect(Category, "SERVICES"),
         !str_detect(Category, "SERVICING"),
         !str_detect(Category, "REPAIR"),
         !str_detect(Category, "GASOLINE"),
         !str_detect(Category, "FARE"),
         !str_detect(Category, "EARNINGS"),
         !str_detect(Category, "VEHICLES"),
         !str_detect(Category, "UNSAMPLED ITEMS"),
         !str_detect(Category, "FEES"),
         !str_detect(Category, "EXPENSES"),
         !str_detect(Category, "RENT"),
         !str_detect(Category, "RENTAL"),
         !str_detect(Category, "CAR"),
         !str_detect(Category, "TRUCK"),
         !str_detect(Category, "SERVICE"),
         !str_detect(Category, "EARNINGS")) %>%
  mutate(
    Category = str_replace(Category, " \\(EXCLUDING FROZEN\\)", ""),
    Category = str_replace(Category, " AT HOME", ""),
    Category = str_replace(Category, "UNCOOKED", "")
  )

items <- c("flour", "cereal", "rice", "pasta", "cornmeal", "bread", "biscuits", "muffins", "cake", "cupcakes", "cookies", "crackers", "coffee cake", "doughnuts", "pie", "tart", "ground beef", "beef roast", "beef steak", "ham", "pork chops", "frankfurters", "lunchmeat", "lamb", "chicken", "fish", "seafood", "eggs", "milk", "apples", "bananas", "citrus fruit", "potatoes", "lettuce", "tomatoes", "frozen fruit", "frozen vegetables", "coffee", "tea", "sugar", "candy", "butter", "margarine", "mayonnaise", "salad dressing", "soup", "dried fruit", "snacks", "olives", "pickles", "sauce", "baby food", "salad", "spirits", "wine", "alcoholic beverage", "oil", "curtains", "drapes", "linens", "bathroom linens", "bedroom linens", "kitchen linens", "dining room linens", "mattress", "sofa", "furniture cover", "decorative pillows", "chairs", "tables", "kitchen furniture", "dining room furniture", "infants furniture", "outdoor furniture", "refrigerators", "home freezer", "laundry machine", "drier", "range", "cooktop", "microwave", "kitchen appliances", "dishwashwer", "lamps", "lighting fixture", "clock", "plant", "flowers", "dishes", "flatware", "tableware", "kitchenware", "paint", "wallpaper", "power tools", "handtools", "cleaning supplies", "cleaning products", "household paper", "moving boxes", "men's suits", "sport coat", "men's outerwear", "men's underwear", "men's activewear", "men's shirt", "men's sweater", "men's vest", "men's pants", "men's shorts", "boy outerware", "boys shirts", "boys sweaters", "boys underwear", "boys suits", "boys pants", "boys sportwear", "women outerwear", "women dresses", "womens tops", "skirts", "women's pants", "womens shorts", "women's suits", "women's hosiery", "women's sportswear", "girls outerwear", "girls dresses", "girls tops", "girls skirts", "girls pants", "girls shorts", "girls sportswear", "girls underwear", "men's footwear", "boys footwear", "girls footwear", "women's footwear", "infant clothing", "infant outerwear", "toddler clothing", "toddler outerwear", "watches", "jewelry", "tires", "motor oil", "coolant", "prescription drugs", "drugs", "nonprescription drugs", "first-aid", "televisions", "DVDs", "Pet food", "pet supplies", "bicycles", "hunting", "fishing", "camping", "film", "photography", "toys", "games", "playground", "video games", "software", "sewing", "music", "music instruments", "magazines", "books", "school supplies", "computer", "phone", "smartphone", "cigarettes", "shaving", "cosmetics", "perfume", "nails", "stationary", "gift wrap", "wrapping paper", "luggage")
