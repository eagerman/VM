struct _item {
    int price;
    int quantity;
}

Setup () {
    setup_initial_inventory();
    display_the_start_screen_with_text();
}

loop () {
    if(keypress >= 1 && <= 9){
        selection_screen(keypress);
    } else if (keypress == * && time > 5) {
        admin_screen();
    }
}

selection_screen(int keypress) {
    item = keypress;
    if(item.quantity = 0) {
        emptyScreen(item);
    } else {
        getItem(item)
    }
}

emptyScreen(int item_num) {
    while(time < 3) {
        if(pushbutton) {
            return;
        }
        if(time < 1.5) {
            lights_on;
        }
        print(Out of stock);
        print(item_num);
    }
}

getItem(int item) {
    while(coins > 0 || !#) {
        lightled = paid_ammount;
        if(paying()) {
            coins--;
        } else if(quit) {
            coin_return();
        }
        print(insert coins);
        print(coins)
    }
    if(coins == 0) {
        retrieve_item(item);
    }
}

paying() {
    if(pay) {
        return true;
    }
    return false;
}

coin_return() {
    return coins actions
}

retrieve_item(item) {
    item.quantity--;
}