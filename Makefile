
all: chats chat

chat: Chat.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

chats: Chats.hs
	$(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f Chats.hi Chats.o chats
	rm -f Chat.hi Chat.o chat

HC=ghc
