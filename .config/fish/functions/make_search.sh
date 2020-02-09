function make_search --description 'search for ports using make search name'
    command make -C $PORTSDIR search name=$argv
