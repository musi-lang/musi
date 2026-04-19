template <class T>
struct Show {
    virtual std::string show(const T& value) const = 0;
};
